{-# LANGUAGE OverloadedStrings #-}

module JavaScript.CssElementQueries where

import           GHCJS.DOM
import           GHCJS.DOM.HTMLElement
import           GHCJS.Foreign
import           JavaScript.CssElementQueries.Internal


onElementResize :: IsHTMLElement elem => elem -> IO () -> IO (IO ())
onElementResize s h = do
  let ref = toHTMLElement s
  cb <- asyncCallback AlwaysRetain h
  sensor <- ceq_attach ref cb
  return $ ceq_detach sensor >> release cb
