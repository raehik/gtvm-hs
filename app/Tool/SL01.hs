module Tool.SL01 where

import           GHC.Generics
import           Control.Monad.IO.Class
import           Common.Config

data Cfg = Cfg
  { cfgIn          :: Stream 'StreamIn  "data"
  , cfgOut         :: Stream 'StreamOut "data"
  } deriving (Eq, Show, Generic)

run :: MonadIO m => Cfg -> m ()
run _ = liftIO $ putStrLn "TODO"
