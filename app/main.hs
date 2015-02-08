{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative

import qualified Data.Text                       as T
import           Data.Time
import           FRP.Sodium
import           JavaScript.ECDIS.GeneralWidgets
import           JavaScript.ECDIS.Map
import           JavaScript.ECDIS.SystemClock
import           JavaScript.JQuery               hiding (Event)
import           Wavecore.ECDIS.Clock
import           Wavecore.ECDIS.Map


main :: IO ()
main = do
  -- the system clock
  (sysclk, delSysClock) <- mkSystemClock 1000000

  -- the clock element selector
  selClock <- select "*[role='ecdis_clock']"

  -- button to switch between utc/local time
  --(eSWClock, delSWClock) <- reactiveButton "UTC/LT" (pure ()) selClock
  let delSWClock :: IO ()
      delSWClock = return ()
  let (eSWClock) = never -- TODO event for switch
      bTZ = pure utc -- TODO find timezone for current position

  -- the switch clock
  swclk <- sync $ clock sysclk bTZ >>= switchClock eSWClock

  -- connect a time field to switch clock
  delTField <- reactiveTimeField "%X%Z" (_bSwitchClockTime swclk) selClock

  -- window resize event
  (bWindowSize, delWindowSize) <- windowSizeB

  -- connect the map
  selMap <- select "*[role='ecdis_map']"
  let bZoom = pure $ 25000 -- TODO Zoom
      bCoord = pure $ dmsCoord (52,23,42) (7,42,23) -- TODO Pos
  (mapObj, delMap) <- mkMap bZoom bCoord bWindowSize selMap


  mpos <- select "*[role='ecdis_mouse_pos']"
  _ <- sync $ listen (_mapPointer mapObj) $
       \v -> case v of
              MouseDown v' -> setText (T.pack $ show v') mpos >> return ()
              MouseUp _ -> return ()


{-
  onBeforeUnload $ do
    delWindowSize
    delTField
    delSWClock
    delSysClock
  --  delMap
-}
  return ()



  -- cleanup handler
