{-# LANGUAGE OverloadedStrings #-}

module JavaScript.SVG
       ( svgPoint, svgGetScreenCTM
       , SVGPoint
       , svgPointGetX, svgPointGetY, svgPointSetX, svgPointSetY
       , SVGMatrix
       , cursorPoint
       , jq_offset, JQOffset, jqOffsetGetLeft, jqOffsetGetTop
       , dom_getBoundingClientRect, TextRectangle, trLeft
       ) where

import           JavaScript.JQuery
import           JavaScript.SVG.Internal

svgPointGetX :: SVGPoint -> IO Double
svgPointGetX p = do
  vM <- _objGet "x" p
  case (vM) of
   Nothing -> fail "svgPointGetX: unable to get x"
   Just x -> return x

svgPointGetY :: SVGPoint -> IO Double
svgPointGetY p = do
  vM <- _objGet "y" p
  case (vM) of
   Nothing -> fail "svgPointGetY: unable to get y"
   Just y -> return y

svgPointSetX :: Double -> SVGPoint -> IO ()
svgPointSetX = _objSet "x"

svgPointSetY :: Double -> SVGPoint -> IO ()
svgPointSetY = _objSet "x"

cursorPoint :: JQuery -> Double -> Double -> IO (Double, Double)
cursorPoint svg x y = do
  p <- svgPoint svg
  svgPointSetX x p
  svgPointSetY y p
  p' <- svgGetScreenCTM svg >>= svgMatrixInverse >>= svgPointMatrixTransform p
  x' <- svgPointGetX p'
  y' <- svgPointGetY p'
  return (x',y')

jqOffsetGetLeft :: JQOffset -> IO Int
jqOffsetGetLeft p = do
  vM <- _objGet "left" p
  case (vM) of
   Nothing -> fail "jqOffsetGetLeft: unable to get left"
   Just x -> return x

jqOffsetGetTop :: JQOffset -> IO Int
jqOffsetGetTop p = do
  vM <- _objGet "top" p
  case (vM) of
   Nothing -> fail "jqOffsetGetTop: unable to get top"
   Just x -> return x

trLeft :: TextRectangle -> IO (Maybe Int)
trLeft p = _objGet "left" p
