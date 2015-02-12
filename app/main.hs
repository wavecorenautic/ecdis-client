{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Monad.Trans       (liftIO)
import           Data.Text                 (Text)
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLBodyElement
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           JavaScript.ECDIS.SeaMap
import           JavaScript.ECDIS.Widget


main :: IO ()
main = runWebGUI $ \ webView -> do
  enableInspector webView
  print ("ecdis startup ..." :: Text)
  doc <- fmap (maybe (error "ecdis: unable to get dom document")
               castToHTMLDocument) $
         webViewGetDomDocument webView
  body <- fmap (maybe (error "ecdis: unable to get dom body")
               id) $
          documentGetBody doc

  -- find dom nodes
  seaMapNode <-
    fmap (maybe (error "ecdis: unable to find ecdis_seamap element")
          castToHTMLElement) $
    documentGetElementById doc ("seamap1" :: Text)
  seaMapZoomNode <-
    fmap (maybe (error "ecdis: unable to find ecdis_seamap element")
          castToHTMLElement) $
    documentGetElementById doc ("seamap1_ctrls" :: Text)

  -- initialize widgets
  seaMapZoomW <- mkSeaMapZoomWidget doc seaMapZoomNode
  seaMapW <- mkSeaMapWidget doc seaMapNode

  -- register cleanup
  let cleanup = liftIO $ do
       print ("ecdis shutdown ..." :: Text)
       widgetCleanup seaMapW
       widgetCleanup seaMapZoomW
       return ()
  _ <- htmlBodyElementOnbeforeunload (castToHTMLBodyElement body) cleanup


  print ("ecdis startup done ..." :: Text)
  return ()



  -- cleanup handler
