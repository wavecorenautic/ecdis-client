{-# LANGUAGE OverloadedStrings #-}

module JavaScript.ECDIS.Map where

import Control.Monad
--import Control.Applicative
import FRP.Sodium
import Numeric.Units.Dimensional.TF.Quantities
import JavaScript.JQuery      hiding (Event)
import JavaScript.CssElementQueries
import JavaScript.SVG
import Wavecore.ECDIS.Map


mkMap ::  Behavior Int -> Behavior (PlaneAngle Double, PlaneAngle Double) ->
         Behavior (Int,Int) -> JQuery -> IO (SeaMap Double Int, IO ())
mkMap bZoom bCoord bWS par = do
  svg <- select mapTemplate
  void $ appendJQuery svg par 
  let getSVGS = do
        svgW <- getWidth svg
        svgH <- getHeight svg
        return $ (round svgW, round svgH)
        
  let handlerWS (_, h) =
        let nh = if (h > 280 ) then (mapfactor * fromIntegral h) else 280
        in do
          void $ setHeight nh par          
  delWSHdlr <- sync $ listen (value bWS) handlerWS

  d0 <- getSVGS
  (bMapSize, bMapSizePush) <- sync $ newBehavior d0  
  delRSevent <- onResize par $ do
    d <- getSVGS
    sync $ bMapSizePush d

  let mapObj = newSeaMap bMapSize bZoom bCoord
      viewBox = seaMapViewBox mapObj
      
  delUpdateViewBox <- sync $ listen (value viewBox) $
                      \vb -> void $ setAttr "viewbox" vb svg
  return (mapObj, do delUpdateViewBox >> delRSevent >> delWSHdlr)
    
  where
    mapfactor = 0.72
    mapTemplate = "<svg style='min-width:250px;min-height:250px;height:100%;width:100%' />"
