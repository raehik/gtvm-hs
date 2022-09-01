{-# LANGUAGE OverloadedStrings #-}

{- | Intent: A sum type with a constructor corresponding to each SCP macro that
     stores user-facing text, with fields to allow checking & replacing such
     text.

We need sum types if we want to handle everything in one place. CSVs don't
support sum types. YAML does. And with sum types, we can also generate
comments from a source SCP -- to e.g. say when another SCP is loaded.

TODO
  * Aeson won't ever omit fields for generic parsing or serializing, except in
    the specific case where you have a concrete @'Maybe' a@. To work around
    that, I need to write a separate, structurally simplified type, which can be
    used for the JSON, and converted to the more powerful internal data type for
    operating on.
-}

module GTVM.SCP.TL where

import GTVM.SCP

import GTVM.Internal.Json
import Util.Text ( tshow )
import Data.Aeson qualified as Aeson

import Strongweak

import GHC.Generics ( Generic )

import Data.Text ( Text )
import Data.Text qualified as Text
import Data.Map ( Map )
import Data.Map qualified as Map
import Data.Char qualified
import Data.Maybe ( fromMaybe )
import Data.Functor.Identity
import Data.Functor.Const

import Control.Monad.State

import Numeric.Natural ( Natural )

type Seg' = Seg 'Weak Text
type SCP' = [Seg']

type SCPTL f a = [TLSeg f a]
type SCPTL' = SCPTL Identity Text

data Env = Env
  { envPendingPlaceholder :: Text

  , envSpeakerIDMap       :: Natural -> Maybe Text
  -- ^ Attempt to obtain a pretty speaker name from an ID.
  --
  -- This data isn't stored in the repo, and must instead be parsed at runtime.

  } deriving (Generic)

data TLSeg f a
  = TLSegTextbox'  (TLSegTextbox f a)
  | TLSegChoice'   [TLSegChoice f a]
  | TLSeg22Choice' (TLSeg22 f a)
  | TLSeg35Choice' (TLSegChoice f a)
  | TLSegComment'  TLSegComment
    deriving (Generic)

deriving instance (Eq   (f a), Eq   a) => Eq   (TLSeg f a)
deriving instance (Show (f a), Show a) => Show (TLSeg f a)

deriving instance Functor     f => Functor     (TLSeg f)
deriving instance Foldable    f => Foldable    (TLSeg f)
deriving instance Traversable f => Traversable (TLSeg f)

jcTLSeg :: Aeson.Options
jcTLSeg = Aeson.defaultOptions
  { Aeson.constructorTagModifier = map Data.Char.toLower . init . drop 5
  , Aeson.sumEncoding = Aeson.TaggedObject
    { Aeson.tagFieldName = "type"
    , Aeson.contentsFieldName = "contents" }}

instance (ToJSON   (f a), ToJSON   a) => ToJSON   (TLSeg f a) where
    toJSON     = genericToJSON     jcTLSeg
    toEncoding = genericToEncoding jcTLSeg
instance (FromJSON (f a), FromJSON a) => FromJSON (TLSeg f a) where
    parseJSON  = genericParseJSON  jcTLSeg

data TLSegComment = TLSegComment
  { scpTLCommentCommentary :: [Text]
  , scpTLCommentMeta       :: Map Text Text
  } deriving stock (Generic, Eq, Show)

instance ToJSON   TLSegComment where
    toJSON     = gtjg "scpTLComment"
    toEncoding = gteg "scpTLComment"
instance FromJSON TLSegComment where
    parseJSON  = gpjg "scpTLComment"

data TLSegTextbox f a = TLSegTextbox
  { tlSegTextboxSource      :: f a
  , tlSegTextboxTranslation :: a
  , tlSegTextboxOverflow    :: Maybe a
  } deriving (Generic)

deriving instance (Eq   (f a), Eq   a) => Eq   (TLSegTextbox f a)
deriving instance (Show (f a), Show a) => Show (TLSegTextbox f a)

deriving instance Functor     f => Functor     (TLSegTextbox f)
deriving instance Foldable    f => Foldable    (TLSegTextbox f)
deriving instance Traversable f => Traversable (TLSegTextbox f)

instance (ToJSON   (f a), ToJSON   a) => ToJSON   (TLSegTextbox f a) where
    toJSON     = gtjg "tlSegTextbox"
    toEncoding = gteg "tlSegTextbox"
instance (FromJSON (f a), FromJSON a) => FromJSON (TLSegTextbox f a) where
    parseJSON  = gpjg "tlSegTextbox"

data TLSegChoice f a = TLSegChoice
  { tlSegChoiceSource :: f a
  , tlSegChoiceTranslation :: a
  } deriving (Generic)

deriving instance (Eq   (f a), Eq   a) => Eq   (TLSegChoice f a)
deriving instance (Show (f a), Show a) => Show (TLSegChoice f a)

deriving instance Functor     f => Functor     (TLSegChoice f)
deriving instance Foldable    f => Foldable    (TLSegChoice f)
deriving instance Traversable f => Traversable (TLSegChoice f)

instance (ToJSON   (f a), ToJSON   a) => ToJSON   (TLSegChoice f a) where
    toJSON     = gtjg "tlSegChoice"
    toEncoding = gteg "tlSegChoice"
instance (FromJSON (f a), FromJSON a) => FromJSON (TLSegChoice f a) where
    parseJSON  = gpjg "tlSegChoice"

data TLSeg22 f a = TLSeg22
  { tlSeg22TopicSource      :: f a
  , tlSeg22TopicTranslation :: a
  , tlSeg22Choices          :: [TLSegChoice f a]
  } deriving (Generic)

deriving instance (Eq   (f a), Eq   a) => Eq   (TLSeg22 f a)
deriving instance (Show (f a), Show a) => Show (TLSeg22 f a)

deriving instance Functor     f => Functor     (TLSeg22 f)
deriving instance Foldable    f => Foldable    (TLSeg22 f)
deriving instance Traversable f => Traversable (TLSeg22 f)

instance (ToJSON   (f a), ToJSON   a) => ToJSON   (TLSeg22 f a) where
    toJSON     = gtjg "tlSeg22"
    toEncoding = gteg "tlSeg22"
instance (FromJSON (f a), FromJSON a) => FromJSON (TLSeg22 f a) where
    parseJSON  = gpjg "tlSeg22"

genTL :: Env -> SCP' -> [TLSeg Identity Text]
genTL env = concatMap go
  where
    go = \case
      -- Segments that contain text to translate. Some generation functions also
      -- handle the commentary, some are plain combinators.
      Seg05 tb  -> genTLTextbox env tb
      Seg09Choice csi (AW32Pairs cs) -> genTLChoiceOuter env csi (map fst cs)
      Seg22 s (AW32Pairs cs) -> [TLSeg22Choice' $ genTL22Choices env s (map fst cs)]
      Seg35 c -> [TLSeg35Choice' $ genTLChoice env c]

      -- Extra segments that are useful to know the presence of.
      Seg0B csi ci ->
        [ meta [ "Choice jump below. Check which choice & choice selection this corresponds to." ]
               [ ("choice_selection_index", tshow csi)
               , ("choice_index", tshow ci) ] ]
      Seg07SCP scp ->
        [ meta [ "Script jump. Any following text is likely accessed by a choice." ]
               [ ("scp_jump_target", scp) ] ]
      Seg0CFlag{} ->
        [ meta [ "0C command here. Alters flow (perhaps checks a flag)." ]
               [] ]

      -- Don't care about the rest.
      _ -> []

genTLTextbox :: Env -> Seg05Text 'Weak Text -> [TLSeg Identity Text]
genTLTextbox env tb =
  [ TLSegComment' ( TLSegComment
    { scpTLCommentCommentary = []
    , scpTLCommentMeta       =
        let speakerName = fromMaybe "N/A" $ envSpeakerIDMap env $ seg05TextSpeakerID tb
         in Map.singleton "speaker" speakerName } )
  , TLSegTextbox'
    ( TLSegTextbox
      { tlSegTextboxSource      = Identity $ seg05TextText tb
      , tlSegTextboxTranslation = envPendingPlaceholder env
      , tlSegTextboxOverflow    = Nothing } )
  ]

genTLChoiceOuter :: Env -> Natural -> [Text] -> [TLSeg Identity Text]
genTLChoiceOuter env csi cs =
  [ meta [ "Choice selection below. Script flow jumps depending on selection." ]
         [ ("choice_selection_index", tshow csi) ]
  , TLSegChoice' $ map (genTLChoice env) cs ]

genTLChoice :: Env -> Text -> TLSegChoice Identity Text
genTLChoice env c = TLSegChoice
                      { tlSegChoiceTranslation = envPendingPlaceholder env
                      , tlSegChoiceSource      = Identity c }

genTL22Choices :: Env -> Text -> [Text] -> TLSeg22 Identity Text
genTL22Choices env s ss = TLSeg22
  { tlSeg22TopicSource = Identity s
  , tlSeg22TopicTranslation = envPendingPlaceholder env
  , tlSeg22Choices = map (genTLChoice env) ss }

meta :: [Text] -> [(Text, Text)] -> TLSeg c s
meta cms kvs = TLSegComment' $ TLSegComment
    { scpTLCommentCommentary = cms
    , scpTLCommentMeta = Map.fromList kvs }

data Error
  = ErrorTLSegOverlong
  | ErrorSourceMismatch
  | ErrorTLSegTooShort
  | ErrorTypeMismatch
  | ErrorUnimplemented
    deriving (Generic, Eq, Show)

apply :: SCP' -> [TLSeg Identity Text] -> Either Error SCP'
apply scp scptl =
    let (scpSegsTled, scptl') = runState (traverseM applySeg scp) scptl
     in case scpSegsTled of
          Left err -> Left err
          Right scpSegsTled' ->
            let scptl'' = skipToNextTL scptl'
             in case scptl'' of
                  _:_ -> Left ErrorTLSegOverlong
                  [] -> Right $ concat scpSegsTled'

skipToNextTL :: [TLSeg c a] -> [TLSeg c a]
skipToNextTL = \case []   -> []
                     a:as -> case a of
                               TLSegComment'{} -> skipToNextTL as
                               _ -> a:as

-- Using highly explicit/manual prisms here. Could clean up.
applySeg
    :: MonadState [TLSeg Identity Text] m
    => Seg' -> m (Either Error [Seg'])
applySeg = \case
  Seg05 tb -> tryApplySeg tryExtractTextbox (tryApplySegTextbox tb)
  Seg09Choice n (AW32Pairs cs) -> tryApplySeg tryExtractChoice (tryApplySegChoice n cs)
  Seg22 topic (AW32Pairs cs) -> tryApplySeg tryExtract22 (tryApplySeg22 topic cs)
  Seg35 a -> tryApplySeg tryExtract35 (tryApplySeg35 a)
  seg -> return $ Right [seg]

tryExtractTextbox :: TLSeg c a -> Maybe (TLSegTextbox c a)
tryExtractTextbox = \case TLSegTextbox' a -> Just a
                          _               -> Nothing

tryExtractChoice :: TLSeg c a -> Maybe [TLSegChoice c a]
tryExtractChoice = \case TLSegChoice' a -> Just a
                         _              -> Nothing

tryExtract22 :: TLSeg c a -> Maybe (TLSeg22 c a)
tryExtract22 = \case TLSeg22Choice' a -> Just a
                     _                -> Nothing

tryExtract35 :: TLSeg c a -> Maybe (TLSegChoice c a)
tryExtract35 = \case TLSeg35Choice' a -> Just a
                     _                -> Nothing

tryApplySeg
    :: MonadState [TLSeg Identity Text] m
    => (TLSeg Identity Text -> Maybe a)
    -> (a -> Either Error [Seg'])
    -> m (Either Error [Seg'])
tryApplySeg f1 f2 = do
    (skipToNextTL <$> get) >>= \case
      []     -> return $ Left ErrorTLSegTooShort
      tl:tls -> do
        put tls
        case f1 tl of
          Nothing -> return $ Left ErrorTypeMismatch
          Just a  -> return $ f2 a

tryApplySegTextbox
    :: Seg05Text 'Weak Text -> TLSegTextbox Identity Text
    -> Either Error [Seg']
tryApplySegTextbox tb tbTL
  | seg05TextText tb /= runIdentity (tlSegTextboxSource tbTL) = Left ErrorSourceMismatch
  | otherwise = Right $ Seg05 tb' : overflow
  where
    tb' = tb { seg05TextText = tlSegTextboxTranslation tbTL }
    overflow = maybe [] fakeTextboxSeg $ tlSegTextboxOverflow tbTL
    fakeTextboxSeg text =
        [Seg05 $ tb { seg05TextVoiceLine = Text.empty
                        , seg05TextText      = text } ]

tryApplySegChoice
    :: Natural -> [(Text, Natural)] -> [TLSegChoice Identity Text]
    -> Either Error [Seg']
tryApplySegChoice n cs csTL
  | length cs /= length csTL = Left ErrorSourceMismatch
  -- lol whatever XD
  | not (and (map (uncurry (==)) checks)) = Left ErrorSourceMismatch
  | otherwise = Right [Seg09Choice n (AW32Pairs edited)]
  where
    checks = zip (map (runIdentity . tlSegChoiceSource) csTL) (map fst cs)
    edited = zip (map tlSegChoiceTranslation csTL) (map snd cs)

tryApplySeg22
    :: Text -> [(Text, Natural)] -> (TLSeg22 Identity Text)
    -> Either Error [Seg']
tryApplySeg22 topic cs segTL
  | topic /= runIdentity (tlSeg22TopicSource segTL) = Left ErrorSourceMismatch
  | length cs /= length csTL = Left ErrorSourceMismatch
  -- lol whatever XD
  | not (and (map (uncurry (==)) checks)) = Left ErrorSourceMismatch
  | otherwise = Right [Seg22 (tlSeg22TopicTranslation segTL) (AW32Pairs edited)]
  where
    csTL = tlSeg22Choices segTL
    checks = zip (map (runIdentity . tlSegChoiceSource) csTL) (map fst cs)
    edited = zip (map tlSegChoiceTranslation csTL) (map snd cs)

tryApplySeg35
    :: Text -> TLSegChoice Identity Text
    -> Either Error [Seg']
tryApplySeg35 a aTL
  | a /= runIdentity (tlSegChoiceSource aTL) = Left ErrorSourceMismatch
  | otherwise = Right [Seg35 (tlSegChoiceTranslation aTL)]

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f v'))
    -> t v
    -> m (f (t v'))
traverseM f xs = sequenceA <$> traverse f xs

-- | Field ordering. To be used for pretty printing 'TLSeg's.
--
-- TODO use \cases on GHC 9.4
tlSegFieldOrdering :: Text -> Text -> Ordering
tlSegFieldOrdering = go
  where
    go "type" _ = LT
    go _ "type" = GT
    go "source" _ = LT
    go _ "source" = GT
    go "translation" _ = LT
    go _ "translation" = GT
    go "meta" _ = LT
    go _ "meta" = GT
    go s1 s2 = compare s1 s2

--------------------------------------------------------------------------------

segIsTlTarget :: Seg f a -> Bool
segIsTlTarget = \case
  Seg05{}       -> True
  Seg09Choice{} -> True
  Seg22{}       -> True
  Seg35{}       -> True
  _             -> False

-- TODO isn't there an easier way to define this??? natural transformation????
segDropMeta :: TLSeg f a -> TLSeg (Const ()) a
segDropMeta = \case
  TLSegTextbox'  (TLSegTextbox _ t o) ->
    TLSegTextbox' $ TLSegTextbox (Const ()) t o
  TLSegChoice'   cs -> TLSegChoice' $ map handleChoice cs
  TLSeg22Choice' (TLSeg22 _ t cs) ->
    TLSeg22Choice' $ TLSeg22 (Const ()) t $ map handleChoice cs
  TLSeg35Choice' c ->
    TLSeg35Choice' $ handleChoice c
  TLSegComment' x -> TLSegComment' x
  where
    handleChoice (TLSegChoice _ t) = TLSegChoice (Const ()) t
