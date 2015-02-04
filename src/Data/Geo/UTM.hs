{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Geo.UTM where


import qualified Prelude as P
import Numeric.Units.Dimensional.TF.Prelude

import           Control.Lens ((^.))
import           Data.Geo.Geodetic
import           Data.Geo.Math
import           Data.Geo.TraverseMercator


data ZoneSpec =
  MINPSEUDOZONE |
  INVALID |
  MATCH |
  UTM |
  STANDARD |
  MAXPSEUDOZONE |
  MINZONE |
  UPS |
  MINUTMZONE |
  MAXUTMZONE |
  MAXZONE
  deriving (Show, Eq, Ord)

zoneSpec :: Floating a => ZoneSpec -> Quantity DPlaneAngle a
zoneSpec z = (zoneSpec' z) *~ degree
zoneSpec' :: Num a => ZoneSpec -> a
zoneSpec' MINPSEUDOZONE = P.negate 4
zoneSpec' INVALID = P.negate 4
zoneSpec' MATCH = P.negate 3 
zoneSpec' UTM = P.negate 2
zoneSpec' STANDARD = P.negate 1
zoneSpec' MAXPSEUDOZONE = P.negate 1
zoneSpec' MINZONE = 0
zoneSpec' UPS = 0
zoneSpec' MINUTMZONE = 1
zoneSpec' MAXUTMZONE = 60
zoneSpec' MAXZONE = 60

tile = 100 *~ kilo meter
upseasting = 20 *~ one
utmeasting = 5 *~ one
maxutmSrow = 100 *~ one
minutmNrow = 0 *~ one

falseeasting =
  [ upseasting * tile, upseasting * tile
  , utmeasting * tile, utmeasting * tile ]
  

falsenorthing =
  [ upseasting * tile, upseasting * tile
  , maxutmSrow * tile, minutmNrow * tile ]
  
utmTraverseMercator :: TraverseMercator Double
utmTraverseMercator =
    case (traverseMercator
          ((wgs84 ^. _SemiMajor) *~ meter)
          ((wgs84 ^. _Flattening) *~ one)
          (0.9996 *~ one)
          tmAlphaBeta6) of
      Just tm -> tm
      Nothing -> error "utmTraverseMercator: unable to create kernel for wgs84"
                
utmTauf, utmTaupf :: PlaneAngle Double -> PlaneAngle Double             
utmTauf = tauf utmTraverseMercator
utmTaupf = taupf utmTraverseMercator          

centralMeridian :: (GeoFloat a) => Int ->  PlaneAngle a           
centralMeridian z =  fromIntegral ((P.-) ((P.*) 6 z) 183) *~ degree

latitudeBand :: (GeoFloat a) => PlaneAngle a -> Int
latitudeBand lat =
  let ilat = (floor $ lat /~ degree) :: Int
  in max (-10) $ min 9 $ (P.-) ((P.div) ((P.+) ilat 80) 8) 10
     
standardZone :: (GeoFloat a) => PlaneAngle a -> PlaneAngle a -> Int -> Int
standardZone lat lon setzone
  | (not $ setzone >= (zoneSpec' MINPSEUDOZONE) &&
     setzone <= (zoneSpec' MAXZONE)) = error "illegal zone"
  | (setzone >= (zoneSpec' MINZONE) ||
     setzone == (zoneSpec' INVALID)) = setzone
  | ((isNanD degree lat) || (isNanD degree lon)) = (zoneSpec' INVALID)
  | setzone == (zoneSpec' UTM) ||
    (lat >= ((-80) *~ degree) &&
     lat < (84 *~ degree)) =
      let ilon' = (floor $ lon /~ degree)
          ilon :: Int
          ilon = if (ilon' >= 180)
                 then (P.-) ilon' 360
                 else if (ilon' < (-180))
                      then (P.+) ilon' 360
                      else ilon'
          zone = ((P.+) ilon 186) `div` 6
          band = latitudeBand lat
      in if ((band == 7) && (zone == 31) && ((P.>=) ilon 3))
         then 32
         else if ((band == 9) && ((P.<) ilon 42) && ((P.>=) ilon 0))
              then (P.*) 2 $ (P.+) 1 $ (P.div) 12 $ (P.+) ilon 183
              else zone

  
utmForward' :: PlaneAngle Double -> PlaneAngle Double ->
              Maybe (Int, Bool, TM Double)
utmForward' lat lon =
  let northp1 = lat >= (0 *~ degree)
      zone1 = standardZone lat lon (zoneSpec' UTM)
      lon0 = centralMeridian $ zone1
      dlon' = lon - lon0
      idlon :: Int
      idlon = P.div ((P.+) 180 (floor $ dlon /~ degree)) 360
      fdlon = (fromIntegral idlon) *~ degree
      dlon = abs $ dlon - (360 *~ degree) * fdlon
      utmp = zone1 /= zoneSpec' UPS
      tm = if (utmp)
           then tmForward utmTraverseMercator lon0 lat lon
           else error $ "UPS not implemented yet"
      ind :: Int
      ind = (P.+) (if utmp then 2 else 0) (if northp1 then 1 else 0)
  in if (zone1 == zoneSpec' INVALID) then Nothing
     else Just $ (zone1, northp1
                  tm { _easting = _easting tm + (falseeasting !! ind)
                     , _northing = _northing tm + (falsenorthing !! ind)
                     })
          

_lat_,_lon_ :: PlaneAngle Double
_lat_ = (49 *~ degree) + (29 *~ arcminute) + (13.6 *~ arcsecond)
_lon_ = (8 *~ degree) + (27 *~ arcminute) + (58.6 *~ arcsecond)

--    49° 29′ 13.6″ N
--    8° 27′ 58.6″ E

t = utmForward' _lat_ _lon_

{-                      n = 5762100.5
        e = 397027
        z = "32N"
    in ((x,y), ((n - x, e -y)))
       

-}

--




