module Util where

import           Control.Lens
import           Control.Monad.Reader

-- | Shortcut for @asks . view@.
asks' :: (MonadReader s m) => Getting a s a -> m a
asks' = asks . view
