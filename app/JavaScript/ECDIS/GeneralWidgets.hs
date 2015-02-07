{-# LANGUAGE OverloadedStrings #-}

module JavaScript.ECDIS.GeneralWidgets where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Default
import System.Locale
import FRP.Sodium
import           Control.Monad.IO.Class
import JavaScript.JQuery      hiding (Event)
import           GHCJS.DOM
import           GHCJS.DOM.DOMWindow



windowSizeB :: IO (Behavior (Int, Int), IO ())
windowSizeB = do
  wnd' <- currentWindow
  let wnd = maybe (error "windowResizeB: unable to get currentWindow") id wnd'
  w <- fmap fromIntegral $ domWindowGetInnerWidth wnd
  h <- fmap fromIntegral $ domWindowGetInnerHeight wnd
  (e, del) <- windowResizeE
  b <- sync $ hold (w,h) e
  return (b, del)


windowResizeE :: IO (Event (Int, Int), IO ())
windowResizeE = do
  (e, pushE) <- sync $ newEvent
  wnd' <- currentWindow
  case wnd' of
   Nothing -> return (never, return ())
   Just wnd -> do
     delListen <- domWindowOnresize wnd $ liftIO $ do
       w <- domWindowGetInnerWidth wnd
       h <- domWindowGetInnerHeight wnd
       sync $ pushE (w, h)
     return (e, delListen)

onBeforeUnload :: IO () -> IO ()
onBeforeUnload hdl = do
  wndM <- currentWindow
  case wndM of
   Just wnd -> void $ domWindowOnbeforeunload wnd $ liftIO hdl
   Nothing -> return ()
    


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
