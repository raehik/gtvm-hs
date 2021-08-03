{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TypeOperators      #-}

module GTVM.SCP.Tmp where

import           GTVM.SCP

import           Control.Lens               hiding ((:<))
import           Control.Lens.TH
import           Data.Functor.Foldable.TH
import           Data.Fix
import           Control.Comonad.Cofree

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           GHC.Word

import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Streaming.ByteString       as Q

import           Text.Megaparsec

import qualified Data.Binary.Get as Get

import           Control.Monad.Except

class HasSCPRep a where
    scpBytes :: a -> BS.ByteString

{-
scpBsTest :: Monad m => Q.ByteStream m r -> S.Stream (S.Of (Either Word8 SCPSegment)) m r
scpBsTest = S.unfoldr $ \bs -> do
    unconsed <- Q.uncons bs
    case unconsed of
      Left r         -> return $ Left r
      Right (b, bs') -> do
        case b of
          0x05 -> do
            res <- runExceptT $ scpSegTextboxVoiced bs'
            let res' = rightToMaybe res
            let step =
                    case res' of
                      Nothing          -> (Left b, bs')
                      Just (seg, bs'') -> (Right seg, bs'')
            return $ Right step
          _    -> return $ Right $ (Left b, bs')
  where
    scpSegTextboxVoiced :: Monad m => Q.ByteStream m r -> ExceptT () m (SCPSegment, Q.ByteStream m r)
    scpSegTextboxVoiced bs = do
        (cb1, bs')  <- liftWrapExceptT $ Q.uncons bs
        (cb2, bs'') <- liftWrapExceptT $ Q.uncons bs'
        return (SCPSegTmpTest cb2, bs'')
-}

liftWrapExceptT :: Monad m => m (Either a b) -> ExceptT () m b
liftWrapExceptT f = do
    x <- lift f
    ExceptT . return . emptyEither $ x

emptyEither :: Either a b -> Either () b
emptyEither (Left _)  = Left ()
emptyEither (Right x) = Right x

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

--    asd <- Q.splitAt 1

{-
-- | Lex an SCP bytestream.
scpLex :: Monad m
  => Q.ByteStream m ()
  -> Stream (Of (Either Word8 SCPSegment)) m ()
scpLex = unfoldrBytestream $ \bs -> \case
  0x05 -> do
    Right (cb1, bs')  <- Q.uncons bs
    Right (cb2, bs'') <- Q.uncons bs'
    bs'' <- drop 3 bs'' -- TODO: parse bytes 00 00 00
    -- ...
      return (Right (SCPSegTmpTest 0x05), bs)
  b    -> return (Left b, bs)
-}

unfoldrBytestream
    :: Monad m
    => (Q.ByteStream m r -> Word8 -> m (a, Q.ByteStream m r))
    -> Q.ByteStream m r -> S.Stream (S.Of a) m r
unfoldrBytestream f =
    S.unfoldr $
        \bs -> Q.uncons bs >>= \case
          Left r         -> return $ Left r
          Right (b, bs') -> do
            el <- f bs' b
            return $ Right el

---

data SCP
  = SCPUnknownBytes BS.ByteString SCP
  | SCPEnd

makeBaseFunctor ''SCP

testDataCofree :: Cofree SCPF String
testDataCofree = "one" :< SCPUnknownBytesF "" ("two" :< SCPEndF)

testDataProduct :: AnnSCP String
testDataProduct =
    Fix $ Const "one" :*: SCPUnknownBytesF "" (Fix $ Const "two" :*: SCPEndF)

cofreeToFixConst :: Functor f => Cofree f a -> Fix (Const a :*: f)
cofreeToFixConst = unfoldFix $ \(a :< f) -> Const a :*: f
fixConstToCofree :: Functor f => Fix (Const a :*: f) -> Cofree f a
fixConstToCofree = foldFix $ \(Const a :*: f) -> a :< f

---

-- functor product
data (f :*: g) a = (:*:) { left :: f a, right :: g a }
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type AnnSCP a = Fix (Const a :*: SCPF)
