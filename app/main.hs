{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Geo.Coordinate
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.DOMWindow
import           JavaScript.JQuery      hiding (Event)
import           System.Locale
import           Wavecore.ECDIS.Clock


mkSystemClock :: Int -> IO (ThreadId, Behavior UTCTime)
mkSystemClock f = do
  t0 <- getCurrentTime
  (t, pushT) <- sync $ newBehavior t0
  let timerLoop = do
        threadDelay f
        getCurrentTime >>= sync . pushT
        timerLoop
  tid <- forkIO $ timerLoop
  return (tid, t)


windowSizeB = do
  wnd' <- currentWindow
  let wnd = maybe (error "windowResizeB: unable to get currentWindow") id wnd'
  w <- fmap fromIntegral $ domWindowGetInnerWidth wnd
  h <- fmap fromIntegral $ domWindowGetInnerHeight wnd
  e <- windowResizeE
  sync $ hold (w,h) e


windowResizeE :: IO (Event (Int, Int))
windowResizeE = do
  (e, pushE) <- sync $ newEvent
  wnd' <- currentWindow
  case wnd' of
   Nothing -> return never
   Just wnd -> do
     domWindowOnresize wnd $ liftIO $ do
       w <- domWindowGetInnerWidth wnd
       h <- domWindowGetInnerHeight wnd
       sync $ pushE (w, h)
     return e

mkMap :: Behavior (Int,Int) -> JQuery -> IO (IO ())
mkMap bWS par = do
  svg <- select mapTemplate >>= \s -> appendJQuery s par
  let handler (_, h) =
        let nh = if (h > 280 ) then (mapfactor * fromIntegral h) else 280
        in void $ setHeight nh par
  delWSHdlr <- sync $ listen (value bWS) handler

  return $ do
    delWSHdlr
  where
    mapfactor = 0.72
    mapTemplate = "<svg style='min-width:250px;min-height:250px;height:100%;width:100%' />"

reactiveButton :: Text -> IO t -> JQuery -> IO (Event t, IO ())
reactiveButton label hdlr par = do
  (evt, a) <- sync newEvent
  button <- select "<button />"
  void $ setText label button
  void $ appendJQuery button par
  let handler _ = hdlr >>= sync . a
  delAction <- on handler "click" def button
  return (evt, delAction)



reactiveTimeField :: FormatTime a => String -> Behavior a -> JQuery -> IO (IO ())
reactiveTimeField tfmt bT par =
  let _bTstr  = fmap (T.pack . formatTime defaultTimeLocale tfmt) bT
      _bTstra = fmap (T.pack . formatTime defaultTimeLocale "%X%z") bT
  in do
    field <- select "<time />"
    void $ appendJQuery field par
    delAction' <- fmap sequence . sync . sequence $ [
      listen (value _bTstr) (\t -> do void $ setText t field),
      listen (value _bTstra) (\t -> do void $ setAttr "datetime" t field)
      ]
    return $ (void $ delAction')

main :: IO ()
main = do
  -- the system clock
  (sysclk_tid, sysclk) <- mkSystemClock 1000000

  -- the clock element selector
  selClock <- select "*[role='ecdis_clock']"


  -- button to switch between utc/local time
  --(eSWClock, delSWClock) <- reactiveButton "UTC/LT" (pure ()) selClock
  let eSWClock = never
  let delSWClock = return ()

  -- the switch clock
  swclk <- sync $ clock sysclk (pure utc) >>= switchClock eSWClock

  -- connect a time field to switch clock
  delTField <- reactiveTimeField "%X%Z" (_bSwitchClockTime swclk) selClock

  -- window resize event
  bWindowSize <- windowSizeB

  -- connect the map
  selMap <- select "*[role='ecdis_map']"
  delMap <- mkMap bWindowSize selMap

  let cleanup = do
        -- TODO kill clock thred
        void $ sequence [delTField, delSWClock, delMap]

  yield
