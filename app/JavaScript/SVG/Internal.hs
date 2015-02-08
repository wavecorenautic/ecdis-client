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

data SVGMatrix_
type SVGMatrix = JSRef SVGMatrix_

data JQOffset_
type JQOffset = JSRef JQOffset_

data TextRectangle_
type TextRectangle = JSRef TextRectangle_


foreign import javascript unsafe "$1.createSVGPoint();"
  svgPoint :: JQuery -> IO SVGPoint

_objGet :: (FromJSRef a) => String -> JSRef b -> IO (Maybe a)
_objGet f p = do  
  vref <- getProp (f) p
  fromJSRef vref

_objSet :: (ToJSRef a) => String -> a -> JSRef b -> IO ()
_objSet f v p = do
  vref <- toJSRef v
  setProp f vref p


foreign import javascript unsafe "$r = $1.getScreenCTM();"
  svgGetScreenCTM :: JQuery -> IO SVGMatrix

foreign import javascript unsafe "$r = $1.inverse();"
  svgMatrixInverse :: SVGMatrix -> IO SVGMatrix

foreign import javascript unsafe "$r = $1.matrixTransform($2);"
  svgPointMatrixTransform :: SVGPoint -> SVGMatrix -> IO SVGPoint

foreign import javascript unsafe "$r = $1.offset();"
  jq_offset :: JQuery -> IO JQOffset


foreign import javascript unsafe
  "$r = $1.getBoundingClientRect();"
  dom_getBoundingClientRect :: JQuery -> IO TextRectangle


#endif
