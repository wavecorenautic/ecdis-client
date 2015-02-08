{-# LANGUAGE OverloadedStrings #-}

module JavaScript.ECDIS.Map where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Default
import           FRP.Sodium
import           JavaScript.CssElementQueries
import           JavaScript.JQuery                       hiding (Event)
import qualified JavaScript.JQuery                       as JQ
import           JavaScript.SVG
import           Numeric.Units.Dimensional.TF.Quantities
import           Wavecore.ECDIS.Map


mkLocalClickEvent sel = do
  (pointerE, pushPointerE) <- sync newEvent
  let hdl b e = do
        x <- pageX e
        y <- pageY e
        o <- jq_offset sel
        dx <- fmap fromIntegral $ jqOffsetGetLeft o
        dy <- fmap fromIntegral $ jqOffsetGetTop o
        sync $ pushPointerE (b, (round $ x - dx ,round $ y - dy))
  delMD <- mousedown (hdl True) def sel
  delMU <- mouseup   (hdl False) def sel
  return (pointerE, delMU >> delMD)

mkMap ::  Behavior Int -> Behavior (PlaneAngle Double, PlaneAngle Double) ->
         Behavior (Int,Int) -> JQuery -> IO (SeaMap Double Int, IO ())
mkMap bZoom bCoord bWS par = do

  -- create svg element
  svg <- select mapTemplate
  void $ appendJQuery svg par

  -- window size event
  let handlerWS (_, h) =
        let nh = if (h > 280 ) then (mapfactor * fromIntegral h) else 280
        in do
          void $ setHeight nh par
  delWSHdlr <- sync $ listen (value bWS) handlerWS

  -- map size behavior
  let getSVGS = do
        svgW <- getWidth svg
        svgH <- getHeight svg
        return $ (round svgW, round svgH)
  d0 <- getSVGS
  (bMapSize, bMapSizePush) <- sync $ newBehavior d0
  delRSevent <- onResize par $ do
    d <- getSVGS
    sync $ bMapSizePush d

  (pointerE, delPointerE) <- mkLocalClickEvent svg

  mapObj <- sync $ newSeaMap bMapSize bZoom bCoord pointerE
  let viewBox = seaMapViewBox mapObj


  delUpdateViewBox <- sync $ listen (value viewBox) $
                      \vb -> void $ setAttr "viewbox" vb svg



  return (mapObj, do delUpdateViewBox >> delPointerE >>
                       delRSevent >> delWSHdlr)

  where
    mapfactor = 0.72
    mapTemplate =
      "<svg style='min-width:250px;min-height:250px;height:100%;width:100%' />"
