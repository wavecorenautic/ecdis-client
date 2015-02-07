{-# LANGUAGE OverloadedStrings #-}

module JavaScript.CssElementQueries where

import GHCJS.Foreign
import JavaScript.CssElementQueries.Internal
import JavaScript.JQuery


onResize :: JQuery -> IO () -> IO (IO ())
onResize s h = do
  cb <- asyncCallback AlwaysRetain h
  sensor <- ceq_attach s cb
  return $ ceq_detach sensor >> release cb
