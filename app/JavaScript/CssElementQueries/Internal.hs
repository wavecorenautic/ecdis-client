{-# LANGUAGE OverloadedStrings, EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP #-}


module JavaScript.CssElementQueries.Internal where


import GHCJS.Types
import GHCJS.Foreign
import           GHCJS.DOM
import           GHCJS.DOM.HTMLElement



#ifdef ghcjs_HOST_OS

data ResizeSensor_

type ResizeSensor = JSRef ResizeSensor_


foreign import javascript unsafe "new ResizeSensor($1, $2);"
  ceq_attach ::  HTMLElement -- ^ selector                       
                -> JSFun b -- ^ callback
                -> IO (ResizeSensor)

foreign import javascript unsafe "$1.detach();"                         
  ceq_detach :: ResizeSensor -> IO ()
                      
#endif
