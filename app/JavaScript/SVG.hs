{-# LANGUAGE OverloadedStrings #-}

module JavaScript.SVG
       ( SVGPoint
       , svgPoint, svgPointGetX, svgPointGetY, svgPointSetX, svgPointSetY
       ) where


import JavaScript.SVG.Internal


svgPointGetX :: SVGPoint -> IO Double
svgPointGetX p = do
  vM <- _svgPointGet "x" p
  case (vM) of
   Nothing -> fail "svgPointGetX: unable to get x"
   Just x -> return x

svgPointGetY :: SVGPoint -> IO Double
svgPointGetY p = do
  vM <- _svgPointGet "y" p
  case (vM) of
   Nothing -> fail "svgPointGetY: unable to get y"
   Just y -> return y


svgPointSetX :: Double -> SVGPoint -> IO ()
svgPointSetX = _svgPointSet "x"

svgPointSetY :: Double -> SVGPoint -> IO ()
svgPointSetY = _svgPointSet "x"
