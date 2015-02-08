{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}

module Wavecore.ECDIS.Map ( SeaMap (..), SeaMapPointer (..), newSeaMap, newSeaMap'
                          , seaMapViewBox
                          , dmsCoord, pxPair) where

import           Control.Applicative
import           Data.Geo.Math
import           Data.Geo.TransverseMercator
import           Data.Geo.UTM
import           Data.Maybe
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           FRP.Sodium
import           Numeric.Units.Dimensional.TF.NonSI   (inch)
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P


type SeaMapPointerPos f = GeoFloat f =>
  ((Length f, Length f), (PlaneAngle f, PlaneAngle f))

data SeaMapPointer f =
  MouseDown (SeaMapPointerPos f) |
  MouseUp (SeaMapPointerPos f)


type SeaMapPointerInput f = (Bool,(Dimensionless f, Dimensionless f))


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
    _mapTL :: Behavior (Length f, Length f),
    _mapTLCoord :: Behavior (PlaneAngle f, PlaneAngle f),
    _mapBL :: Behavior (Length f, Length f),
    _mapBLCoord :: Behavior (PlaneAngle f, PlaneAngle f),
    _mapReverse :: Behavior (Length f -> Length f ->
                             Maybe (PlaneAngle f, PlaneAngle f)),
    _mapPxZoom :: Behavior (Length f),
    _mapPointer :: Event (SeaMapPointer f),
    _mapPointerRaw :: Event (SeaMapPointerInput f)
    }

newSeaMap :: (GeoFloat f, Integral i) =>
             Behavior (i, i) ->
             Behavior (i) ->
             Behavior (PlaneAngle f, PlaneAngle f) ->
             Event (Bool, (i,i)) ->
             Reactive (SeaMap f i)
newSeaMap s zoom p pointerE  =
  let s' = liftA (\(w,h) -> (w *~ one, h *~ one)) s
      z' = liftA (\z -> z *~ one) zoom
      pointerE' = fmap (\(b, (x, y)) -> (b, ((fromIntegral x) *~ one
                                           ,(fromIntegral y) *~ one)))
                  pointerE
  in do

    newSeaMap' (pure $ (1 *~ inch) / (96 *~ one)) s' z' p pointerE'



newSeaMap' :: (GeoFloat f, Integral i) =>
              Behavior (Length f) ->
              Behavior (Dimensionless i, Dimensionless i) ->
              Behavior (Dimensionless i) ->
              Behavior (PlaneAngle f, PlaneAngle f) ->
              Event (SeaMapPointerInput f) ->
              Reactive (SeaMap f i)
newSeaMap' pxFactor pxSize zoom center pointerE =
  let _pxZoom = (*) <$> pxFactor <*> fmap (fmap fromIntegral) zoom
      _w = (*) <$> fmap (fmap fromIntegral . fst) pxSize <*> _pxZoom
      _w2 = liftA (\x -> x / _2) _w
      _h = (*) <$> fmap (fmap fromIntegral . snd) pxSize <*> _pxZoom
      _h2 = liftA (\x -> x / _2) _h
      _utm = fmap (fromJust) $
             liftA (\(lat,lon) -> utmForward' lat lon) center
      _tm = fmap snd _utm
      _c = fmap (\tm -> (_easting tm, _northing tm) ) _tm
      _TL = liftA3 (\(cx,cy) w2 h2 -> (cx - w2, cy + h2)) _c _w2 _h2
      _BL = liftA3 (\(cx,cy) w2 h2 -> (cx - w2, cy - h2)) _c _w2 _h2
      _zone = fmap (fst . fst) _utm
      _northp = fmap (snd . fst) _utm
      _utmRv = utmReverse <$> _zone <*> _northp
      _TLC = liftA2 (\f (x,y) -> fromJust $ f x y) _utmRv _TL
      _BLC = liftA2 (\f (x,y) -> fromJust $ f x y) _utmRv _BL
      _pnt2map = _pnt2map' <$> _northp <*> _utmRv <*> _pxZoom <*> _TL
      mkMouse (bDown, pxpos) pmap =
        let res = pmap pxpos
        in if bDown then MouseDown res
           else MouseUp res
      _pointerE  = snapshot mkMouse pointerE _pnt2map
  in do
    return SeaMap {
    _mapPixelSize = pxSize,
    _mapSize = liftA2 (\w h -> (w,h)) _w _h,
    _mapPixelFactor = pxFactor,
    _mapZoom = zoom,
    _mapCenterCoord = center,
    _mapZone = _zone,
    _mapNorth = _northp,
    _mapMeridianConvergence = fmap _meridianConvergence _tm,
    _mapProjectionScale = fmap _projectionScale _tm,
    _mapTL = _TL,
    _mapTLCoord = _TLC,
    _mapBL = _BL,
    _mapBLCoord = _BLC,
    _mapReverse = _utmRv,
    _mapPointerRaw = pointerE,
    _mapPointer = _pointerE,
    _mapPxZoom = _pxZoom
    }


_pnt2map' :: (GeoFloat f) =>
             Bool ->
             (Length f -> Length f -> Maybe (PlaneAngle f, PlaneAngle f)) ->
             Length f ->
             (Length f, Length f) ->
             (Dimensionless f, Dimensionless f) ->
             ((Length f, Length f), (PlaneAngle f, PlaneAngle f))
_pnt2map' _northp _utmF _z (_ax, _ay) (_px, _py)=
  let (px, py) = (_px * _z, _py * _z)
      px' = _ax + px
      py' = if (_northp) then _ay + py else _ay - py
  in ((px', py'), maybe (error "unable to get utm") id $ _utmF px' py')


seaMapViewBox :: (GeoFloat f, Integral i) => SeaMap f i -> Behavior Text
seaMapViewBox sm =
  let _A = _mapBL sm
      _s = _mapSize sm
  in liftA2 (\(ax, ay) (w, h) -> T.pack $
              concat [ show $ ((round $ (ax /~ meter)) :: Int), " "
                     , show $ ((round $ (ay /~ meter)) :: Int), " "
                     , show $ ((round $ (w /~ meter)) :: Int), " "
                     , show $ ((round $ (h /~ meter)) :: Int)
                     ] ) _A _s

pxPair :: (Integral i) => (i,i) -> (Dimensionless i, Dimensionless i)
pxPair (x,y) = (x *~ one, y *~ one)


dmsCoord :: GeoFloat f => (f,f,f) -> (f,f,f) -> (PlaneAngle f,PlaneAngle f)
dmsCoord (latD, latM, latS) (lonD, lonM, lonS) =
  ((latD *~ degree) + (latM *~ arcminute) + (latS *~ arcsecond)
  ,(lonD *~ degree) + (lonM *~ arcminute) + (lonS *~ arcsecond))
