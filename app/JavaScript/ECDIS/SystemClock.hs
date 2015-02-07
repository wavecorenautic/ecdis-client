module JavaScript.ECDIS.SystemClock where

import Control.Concurrent
import Data.Time
import FRP.Sodium


mkSystemClock :: Int -> IO (Behavior UTCTime, IO ())
mkSystemClock f = do
  t0 <- getCurrentTime
  (t, pushT) <- sync $ newBehavior t0
  let timerLoop = do
        threadDelay f
        getCurrentTime >>= sync . pushT
        timerLoop
  tid <- forkIO $ timerLoop
  return (t, killThread tid)

