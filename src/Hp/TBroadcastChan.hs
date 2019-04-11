module Hp.TBroadcastChan
  ( TBroadcastChan
  , unsafeTBroadcastChanToTChan
  , newTBroadcastChan
  , newTBroadcastChanIO
  , dupTBroadcastChan
  , dupTBroadcastChanIO
  , writeTBroadcastChan
  , writeTBroadcastChanIO
  ) where

import Control.Concurrent.STM

newtype TBroadcastChan a
  = TBroadcastChan (TChan a)

-- | Forget that a channel can only be written to.
unsafeTBroadcastChanToTChan :: TBroadcastChan a -> TChan a
unsafeTBroadcastChanToTChan =
  coerce

newTBroadcastChan :: ∀ a. STM (TBroadcastChan a)
newTBroadcastChan =
  coerce @(STM (TChan a)) newTChan

newTBroadcastChanIO :: ∀ a. IO (TBroadcastChan a)
newTBroadcastChanIO =
  coerce @(IO (TChan a)) newTChanIO

dupTBroadcastChan :: ∀ a. TBroadcastChan a -> STM (TChan a)
dupTBroadcastChan =
  coerce @(TChan a -> _) dupTChan

dupTBroadcastChanIO :: TBroadcastChan a -> IO (TChan a)
dupTBroadcastChanIO =
  atomically . dupTBroadcastChan

writeTBroadcastChan :: ∀ a. TBroadcastChan a -> a -> STM ()
writeTBroadcastChan =
  coerce @(TChan a -> _ -> _) writeTChan

writeTBroadcastChanIO :: TBroadcastChan a -> a -> IO ()
writeTBroadcastChanIO chan value =
  atomically (writeTBroadcastChan chan value)
