{-# LANGUAGE OverloadedStrings, EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP #-}


module JavaScript.SVG.Internal where


import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import JavaScript.JQuery hiding (getProp, setProp)



#ifdef ghcjs_HOST_OS

data SVGPoint_

type SVGPoint = JSRef SVGPoint_

foreign import javascript unsafe "$1.createSVGPoint();"
  svgPoint :: JQuery -> IO SVGPoint

_svgPointGet :: (FromJSRef a) => String -> SVGPoint -> IO (Maybe a)
_svgPointGet f p = do  
  vref <- getProp (f) p
  fromJSRef vref

_svgPointSet :: (ToJSRef a) => String -> a -> SVGPoint -> IO ()
_svgPointSet f v p = do
  vref <- toJSRef v
  setProp f vref p

#endif
