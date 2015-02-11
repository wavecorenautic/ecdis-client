{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where
import           Control.Applicative

import           Data.Text                (Text)
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLCollection
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Types          (Node)

import           JavaScript.ECDIS.SeaMap



main :: IO ()
main = runWebGUI $ \ webView -> do
  enableInspector webView
  doc <- fmap (maybe (error "ecdis: unable to get dom document") id) $
         webViewGetDomDocument webView
  body <- fmap (maybe (error "ecdis: unable to get dom body") id) $
          documentGetBody doc

  par <- fmap
         (maybe (error "ecdis: unable to find ecdis_seamap element")
          castToHTMLElement)
         $ documentGetElementById doc ("ecdis_seamap" :: Text)

  seaMapWidget <- mkSeaMapWidget par

  return ()



  -- cleanup handler
