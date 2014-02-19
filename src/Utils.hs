module Utils where

import qualified Control.Monad.Trans.RWS as RWS (RWST, withRWST)

withReaderT :: (r' -> r) -> RWS.RWST r w s m a -> RWS.RWST r' w s m a
withReaderT f r = RWS.withRWST (\ r s -> (f r, s)) r
