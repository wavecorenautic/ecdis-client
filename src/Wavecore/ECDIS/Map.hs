module Wavecore.ECDIS.Map ( SeaMap (..), newSeaMap, newSeaMap'
                          , seaMapViewBox
                          , dmsCoord, pxPair) where

import qualified Prelude as P
import Numeric.Units.Dimensional.TF.Prelude
import Numeric.Units.Dimensional.TF.NonSI (inch)
import FRP.Sodium
import Control.Applicative
import Data.Maybe
import Data.Geo.Math
import Data.Geo.TransverseMercator
import Data.Geo.UTM
import qualified Data.Text as T
import Data.Text (Text)

data SeaMap f i =
  SeaMap {
    _mapPixelSize :: Behavior (Dimensionless i, Dimensionless i),
    _mapSize :: Behavior (Length f, Length f),
    _mapPixelFactor :: Behavior (Length f),
    _mapZoom :: Behavior (Dimensionless i),
    _mapCenterCoord :: Behavior (PlaneAngle f, PlaneAngle f),
    _mapZone :: Behavior Int,
    _mapNorth :: Behavior Bool,
    _mapMeridianConvergence :: Behavior (PlaneAngle f),
    _mapProjectionScale :: Behavior (Dimensionless f),
    _mapA :: Behavior (Length f, Length f),
    _mapACoord :: Behavior (PlaneAngle f, PlaneAngle f),
    _mapB :: Behavior (Length f, Length f),
    _mapBCoord :: Behavior (PlaneAngle f, PlaneAngle f)
    }


seaMapViewBox :: (GeoFloat f) => SeaMap f i -> Behavior Text
seaMapViewBox sm =
  let _A = _mapA sm
      _s = _mapSize sm
  in liftA2 (\(ax, ay) (w, h) -> T.pack $ 
              concat [ show . round $ (ax /~ meter), " "
                     , show . round $ (ay /~ meter), " "
                     , show . round $ (w /~ meter), " "
                     , show . round $ (h /~ meter), " "
                     ] ) _A _s

pxPair :: (Integral i) => (i,i) -> (Dimensionless i, Dimensionless i)
pxPair (x,y) = (x *~ one, y *~ one)


dmsCoord :: GeoFloat f => (f,f,f) -> (f,f,f) -> (PlaneAngle f,PlaneAngle f)
dmsCoord (latD, latM, latS) (lonD, lonM, lonS) =
  ((latD *~ degree) + (latM *~ arcminute) + (latS *~ arcsecond)
  ,(lonD *~ degree) + (lonM *~ arcminute) + (lonS *~ arcsecond))

newSeaMap :: (GeoFloat f, Integral i) =>
             Behavior (i, i) ->
             Behavior (i) ->
             Behavior (PlaneAngle f, PlaneAngle f) -> SeaMap f i
newSeaMap s z p =
  let s' = liftA (\(w,h) -> (w *~ one, h *~ one)) s
      z' = liftA (\z -> z *~ one) z
  in newSeaMap' (pure $ (1 *~ inch) / (96 *~ one)) s' z' p

newSeaMap' :: (GeoFloat f, Integral i) =>
              Behavior (Length f) ->
              Behavior (Dimensionless i, Dimensionless i) ->
              Behavior (Dimensionless i) ->
              Behavior (PlaneAngle f, PlaneAngle f) -> SeaMap f i
newSeaMap' pxFactor pxSize zoom center =
  let a = 0
      _pxZoom = (*) <$> pxFactor <*> fmap (fmap fromIntegral) zoom
      _w = (*) <$> fmap (fmap fromIntegral . fst) pxSize <*> _pxZoom
      _w2 = liftA (\x -> x / _2) _w
      _h = (*) <$> fmap (fmap fromIntegral . snd) pxSize <*> _pxZoom
      _h2 = liftA (\x -> x / _2) _h
      _utm = fmap (fromJust) $
             liftA (\(lat,lon) -> utmForward' lat lon) center
      _tm = fmap snd _utm
      _c = fmap (\tm -> (_easting tm, _northing tm) ) _tm
      _A = liftA3 (\(cx,cy) w2 h2 -> (cx - w2, cy - h2)) _c _w2 _h2
      _B = liftA3 (\(cx,cy) w2 h2 -> (cx + w2, cy + h2)) _c _w2 _h2
      _zone = fmap (fst . fst) _utm
      _northp = fmap (snd . fst) _utm
      _AC = liftA3 (\z n (x,y) -> let tmr = fromJust $ utmReverse' z n x y
                                  in (_rLatitude tmr, _rLongitude tmr ))
            _zone _northp _A
      _BC = liftA3 (\z n (x,y) -> let tmr = fromJust $ utmReverse' z n x y
                                  in (_rLatitude tmr, _rLongitude tmr ))
            _zone _northp _B
  in SeaMap {
    _mapPixelSize = pxSize,
    _mapSize = liftA2 (\w h -> (w,h)) _w _h,
    _mapPixelFactor = pxFactor,
    _mapZoom = zoom,
    _mapCenterCoord = center,
    _mapZone = _zone, 
    _mapNorth = _northp,
    _mapMeridianConvergence = fmap _meridianConvergence _tm,
    _mapProjectionScale = fmap _projectionScale _tm,
    _mapA = _A,
    _mapACoord = _AC,
    _mapB = _B,
    _mapBCoord = _BC
    }
