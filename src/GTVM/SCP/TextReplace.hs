{-| Support for simple SCP translation via replacing the text in an 05 command.

If you wrap the SCP JSON types in your program, you should track an SCP JSON
document and a text replace JSON document together. That way, you can
write a sort of barebones "SCP translation patch file" (without duplicating the
entire scenario), but still show the various scenario info to the user
(background changes, voice lines, ...).
-}

module GTVM.SCP.TextReplace where

import Data.Aeson qualified as Aeson
import Data.Aeson ( ToJSON(..), FromJSON(..)
                  , genericToJSON, genericToEncoding , genericParseJSON )
import GTVM.Common.Json
import GHC.Generics
import GTVM.SCP
import Raehik.Check
import Control.Monad.State
import BLAKE3 qualified as B3
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS

data SCPTextReplace (c :: Check) bs = SCPTextReplace
  { scpTextReplaceCheck    :: CheckRep c bs
  , scpTextReplaceReplace  :: bs
  , scpTextReplaceOverflow :: Maybe [bs]
  } deriving (Generic)

deriving instance (Eq   (CheckRep c bs), Eq   bs) => Eq   (SCPTextReplace c bs)
deriving instance (Show (CheckRep c bs), Show bs) => Show (SCPTextReplace c bs)

-- If GHC was cleverer, I think it could derive these automatically. It appears
-- most likely to be an unhandled case in the Functor derivation (thanks
-- tomsmeding):
-- https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Tc/Deriv/Functor.hs#L508-537
instance Functor (SCPTextReplace 'CheckEqual) where
    fmap f (SCPTextReplace c r o) = SCPTextReplace (f c) (f r) (fmap (fmap f) o)
instance Foldable (SCPTextReplace 'CheckEqual) where
    foldMap f (SCPTextReplace c r o) = f c <> f r <> foldMap (foldMap f) o
instance Traversable (SCPTextReplace 'CheckEqual) where
    traverse f (SCPTextReplace c r o) =
        SCPTextReplace <$> f c <*> f r <*> traverse (traverse f) o

jcSCPTextReplace :: Aeson.Options
jcSCPTextReplace =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length "scpTextReplace"

instance (ToJSON   (CheckRep c bs), ToJSON   bs) => ToJSON   (SCPTextReplace c bs) where
    toJSON     = genericToJSON     jcSCPTextReplace
    toEncoding = genericToEncoding jcSCPTextReplace
instance (FromJSON (CheckRep c bs), FromJSON bs) => FromJSON (SCPTextReplace c bs) where
    parseJSON  = genericParseJSON  jcSCPTextReplace

data Error bs
  = ErrorTooManyReplaces
  | ErrorReplace bs bs
    deriving (Eq, Show, Generic)

-- TODO highly inefficient due to the concat approach -- very stupid, buuuut
--      lets us use traverseM. meh
replaceEqs :: (Eq bs, Monoid bs) => [SCPTextReplace 'CheckEqual bs] -> SCP bs -> Either (Error bs) (SCP bs)
replaceEqs startTrs scp =
    let (mReplaced, trs') = runState (traverseM go scp) startTrs
     in case mReplaced of
          Left  err      -> Left err
          Right replaced ->
            case trs' of
              []  -> Right $ concat $ replaced
              _:_ -> Left ErrorTooManyReplaces
  where
    go = \case
      SCPSeg05Textbox tb ->
        get >>= \case
          []     -> return $ Right $ [SCPSeg05Textbox tb]
          tr:trs -> case replaceEq tr tb of
                      Nothing  ->
                        let trBs = scpTextReplaceCheck tr
                            tbBs = scpSeg05TextboxText tb
                         in return $ Left $ ErrorReplace trBs tbBs
                      Just tb's -> do
                        put trs
                        return $ Right $ SCPSeg05Textbox <$> tb's
      scpSeg -> return $ Right [scpSeg]

-- TODO we ask for a monoid so we can get an empty value LOOOOL gottem
replaceEq :: (Eq bs, Monoid bs) => SCPTextReplace 'CheckEqual bs -> SCPSeg05Textbox bs -> Maybe [SCPSeg05Textbox bs]
replaceEq tr tb
  | scpSeg05TextboxText tb /= scpTextReplaceCheck tr = Nothing
  | otherwise =
        let tb' = tb { scpSeg05TextboxText = scpTextReplaceReplace tr }
         in Just $ tb' : overflow
  where
    overflow =
        case scpTextReplaceOverflow tr of
          Nothing -> []
          Just x  -> (\t -> tbNoVoice { scpSeg05TextboxText = t }) <$> x -- TODO via optics
    tbNoVoice = tb { scpSeg05TextboxVoiceLine = mempty }

-- TODO another binrep-style typeclass I guess bleh (but this time more plain)
eqToHashB3
    :: SCPTextReplace 'CheckEqual              BS.ByteString
    -> SCPTextReplace ('CheckHash 'HashFuncB3) BS.ByteString
-- TODO for some reason, generic-optics is unhappy with changing the containing
--      type using 'over'. so we have to do it manually
--eqToHashB3 = over (the @"scpTextReplaceCheck") (Hash . hashB3)
eqToHashB3 tr = tr { scpTextReplaceCheck = Hash $ hashB3 $ scpTextReplaceCheck tr }

-- I unpack to '[Word8]' then repack to 'ByteString' because the memory library
-- is very keen on complicated unsafe IO. cheers no thanks
hashB3 :: BS.ByteString -> BS.ByteString
hashB3 bs = BS.pack $ BA.unpack $ B3.hash @B3.DEFAULT_DIGEST_LEN [bs]

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f v'))
    -> t v
    -> m (f (t v'))
traverseM f xs = sequenceA <$> traverse f xs
