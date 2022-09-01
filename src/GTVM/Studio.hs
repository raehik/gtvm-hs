{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{- Notes
  * SCPTL comments are fully ignored. They aren't indexable. They should be
    inside the constructors instead.
  * We ignore SCPTL source fields! Use the SCP instead. We can fill them all in
    in a separate pass e.g. before exporting.

TODO

  * handling unexpected index errors with runtime errors. Perhaps upgrade to
    explicitly handled ones via effect. (Impurely, they can be propagated simply
    with the final IO interpreter.)
-}

module GTVM.Studio where

import GTVM.SCP.TL
import GTVM.SCP.TL qualified as Scptl
import GTVM.SCP
import Strongweak

import Polysemy
import Polysemy.State
import Polysemy.State qualified as Polysemy
import Polysemy.Output
import Polysemy.Output qualified as Polysemy

import Path qualified
import Path ( Path, Rel, Abs, File, Dir, (</>) )

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text ( Text )
import Numeric.Natural
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Functor.Const
import Data.Yaml qualified as Yaml
import GHC.Generics ( Generic )
import Data.String ( IsString )

newtype ScpId = ScpId { getScpId :: Text }
    deriving (Eq, IsString) via Text
    deriving stock Show

-- | Yes, we're ignoring the source fields. We'll fill them in during an export
--   pass. Easier on the brain.
type TLSeg' = TLSeg (Const ()) Text

data Studio m a where
    WriteTl :: TLSeg' -> Studio m ()

    ReadTl :: Studio m (Maybe TLSeg')
    -- ^ 'Nothing' if no entry yet. (Perhaps upgrade 'Data.Text.empty' to
    --   'Nothing'.)

    JumpTlInit :: Studio m ()
    -- ^ Jump to the first translation target SCP command and reset.

    NextTl :: Studio m ()
    -- ^ Step to the next translation target SCP command.

    PrevTl :: Studio m ()
    -- ^ Step to the previous translation target SCP command.

    LoadScp :: ScpId -> Studio m ()
    -- ^ Load the requested SCP.

    GenerateFreshScpTl :: Path Rel Dir -> Studio m ()
    -- ^ Generate a fresh SCPTL for the currently loaded SCP and place in the
    --   the requested studio folder.

    LoadScpTl :: Path Rel Dir -> Studio m ()
    -- ^ Load the SCPTL for the currently loaded SCP, stored at the requested
    --   studio folder.

    SaveScpTl :: Path Rel Dir -> Studio m ()
    -- ^ Save the currently loaded SCPTL to disk.

makeSem ''Studio

-- | Resets and steps to the translation target SCP command with the requested
--   index.
--
-- TODO better behaviour if we jump too far (don't keep spamming 'nextTl')? but
-- this should be prefaced with a UI check that the index exists
jumpTl :: Members '[Studio] r => Natural -> Sem r ()
jumpTl n = do
    jumpTlInit
    replicateM_ (fromIntegral n) nextTl

-- | Generate a fresh SCPTL for the currently loaded SCP, place in the requested
--   studio folder and load immediately.
loadFreshScpTl :: Members '[Studio] r => Path Rel Dir -> Sem r ()
loadFreshScpTl d = generateFreshScpTl d >> loadScpTl d

data St = St
  { stScp      :: [Seg 'Weak Text]
  , stScpIdx   :: Int
  , stScptl    :: Maybe [TLSeg']
  , stScptlIdx :: Int
  , stScpId :: ScpId
  } deriving stock (Generic, Show, Eq)

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

runStudio
    :: Members '[State St, Output Text, Embed IO] r
    => Path Abs Dir -> Sem (Studio ': r) a -> Sem r a
runStudio baseDir = interpret $ \case

  WriteTl tlseg -> do
    st <- get
    case stScptl st of
      Nothing ->
        studioLog "no SCPTL loaded, can't write segment TL"
      Just scptl -> do
        let scptl' = setAt (stScptlIdx st) tlseg scptl
        put st { stScptl = Just scptl' }

  ReadTl -> do
    st <- get
    case stScptl st of
      Nothing -> do
        studioLog "no SCPTL loaded, can't read current segment TL"
        pure Nothing
      Just scptl ->
        let tlSeg = scptl !! stScptlIdx st
        in  if tlSegIsEmpty tlSeg then pure Nothing else pure (Just tlSeg)

  JumpTlInit -> do
    st <- get
    put st { stScpIdx = 0, stScptlIdx = 0 }

  NextTl -> do -- TODO would benefit from lenses
    st <- get
    let scp = stScp st
        scpIdx = stScpIdx st
    if scpIdx >= length scp - 1 then
        -- need more than check to handle length 0,1 cases
        studioLog "at SCP end, can't step any further"
    else do
        let scptlIdx = stScptlIdx st
            (scpIdx', scptlIdx') =
                case scpNextTlIdx scp (scpIdx+1) of
                    Left  scpIdx'' -> (scpIdx'', scptlIdx)
                    Right scpIdx'' -> (scpIdx'', scptlIdx+1)
            st' = st { stScpIdx   = scpIdx'
                     , stScptlIdx = scptlIdx' }
        put st'

  PrevTl -> do -- TODO would benefit from lenses
    st <- get
    let scp = stScp st
        scpIdx = stScpIdx st
    if scpIdx == 0 then
        studioLog "at SCP start, can't rewind any further"
    else do
        let scptlIdx = stScptlIdx st
            (scpIdx', scptlIdx') =
                case scpPrevTlIdx scp (scpIdx-1) of
                    Nothing -> (0, scptlIdx)
                    Just scpIdx'' -> (scpIdx'', scptlIdx-1)
            st' = st { stScpIdx   = scpIdx'
                     , stScptlIdx = scptlIdx' }
        put st'

  LoadScp scpId -> do
    scpFName <- liftIO $ studioYamlRes ".scp" $ getScpId scpId
    let scpFPath = $(Path.mkRelDir "scp") </> scpFName
        fpath = baseDir </> scpFPath
        fpath' = Path.fromAbsFile fpath
    scp <- Yaml.decodeFileThrow fpath'
    modify $ \st -> st { stScpIdx = 0, stScp = scp, stScpId = scpId }

  GenerateFreshScpTl d -> do
    scp <- gets stScp

    scpId <- gets stScpId
    scptlFName <- liftIO $ studioYamlRes ".scptl" $ getScpId scpId
    let scptlFPath = $(Path.mkRelDir "scptl") </> d </> scptlFName
        fpath = baseDir </> scptlFPath
        fpath' = Path.fromAbsFile fpath

    let scptl = genEmptyScptl scp
    liftIO $ Yaml.encodeFile fpath' scptl

  LoadScpTl d -> do

    scpId <- gets stScpId
    scptlFName <- liftIO $ studioYamlRes ".scptl" $ getScpId scpId
    let scptlFPath = $(Path.mkRelDir "scptl") </> d </> scptlFName
        fpath = baseDir </> scptlFPath
        fpath' = Path.fromAbsFile fpath

    scptl <- Yaml.decodeFileThrow fpath'
    modify $ \st -> st { stScptl = Just scptl }

  SaveScpTl d -> do
    st <- get
    case stScptl st of
      Nothing -> pure () -- TODO notify
      Just scptl -> do

        scpId <- gets stScpId
        scptlFName <- liftIO $ studioYamlRes ".scptl" $ getScpId scpId
        let scptlFPath = $(Path.mkRelDir "scptl") </> d </> scptlFName
            fpath = baseDir </> scptlFPath
            fpath' = Path.fromAbsFile fpath

        liftIO $ Yaml.encodeFile fpath' scptl


studioYamlRes :: MonadThrow m => FilePath -> Text -> m (Path Rel File)
studioYamlRes ext res = Path.parseRelFile $ Text.unpack res<>ext<>".yaml"

scpPrevTlIdx :: SCP f a -> Int -> Maybe Int
scpPrevTlIdx scp = go
  where
    go = \case
      -1     -> Nothing
      scpIdx -> if   segIsTlTarget (scp !! scpIdx)
                then Just scpIdx
                else go $ scpIdx-1

scpNextTlIdx :: SCP f a -> Int -> Either Int Int
scpNextTlIdx scp = go
  where
    l = length scp
    go scpIdx =
        if   scpIdx >= l-1
        then Left $ scpIdx-1
        else if   segIsTlTarget (scp !! scpIdx)
             then Right scpIdx
             else go $ scpIdx+1

tlSegIsEmpty :: (Eq a, Monoid a) => TLSeg f a -> Bool
tlSegIsEmpty = \case
  TLSegTextbox' x -> tlSegTextboxTranslation x == mempty -- TODO check overflow
  _ -> False

-- | Required to avoid lots of type annotation, that's life Jim
studioLog :: Member (Output Text) r => Text -> Sem r ()
studioLog = Polysemy.output

genEmptyScptl :: SCP 'Weak Text -> [TLSeg']
genEmptyScptl = map Scptl.segDropMeta . filter (not . isComment) . Scptl.genTL env
  where
    env = Scptl.Env mempty (const Nothing)
    isComment = \case TLSegComment'{} -> True; _ -> False

rLoadAndGenFirstScript :: IO ()
rLoadAndGenFirstScript =
      Polysemy.runFinal
    . Polysemy.embedToFinal @IO
    . runStudioLog Text.putStrLn
    . Polysemy.evalState (St [] 0 Nothing 0 "")
    . runStudio $(Path.mkAbsDir "/home/raehik/sh/gtvm-tl/tooling/gtvm-hs/tmp/studio")
    $ pLoadAndGenFirstScript

--------------------------------------------------------------------------------

runStudioLog
    :: Member (Embed IO) r
    => (o -> IO ()) -> Sem (Output o ': r) a -> Sem r a
runStudioLog f = interpret $ \case
  Output o -> liftIO $ f o

--------------------------------------------------------------------------------

pLoadAndGenFirstScript :: Members '[Studio, Embed IO] r => Sem r ()
pLoadAndGenFirstScript = do
    loadScp "00120zzz0"
    generateFreshScpTl $(Path.mkRelDir "tmp")
    loadScpTl $(Path.mkRelDir "tmp")
    readTl >>= liftIO . print
    writeTl $ tlsegTextbox "ayy lmao"
    readTl >>= liftIO . print
    nextTl
    nextTl
    writeTl $ tlsegTextbox "two ahead"
    saveScpTl $(Path.mkRelDir "tmp")


tlsegTextbox :: a -> TLSeg (Const ()) a
tlsegTextbox a = Scptl.TLSegTextbox' $ Scptl.TLSegTextbox (Const ()) a Nothing
