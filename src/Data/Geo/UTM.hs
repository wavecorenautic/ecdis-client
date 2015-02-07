{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Geo.UTM where


import qualified Prelude as P
import Numeric.Units.Dimensional.TF.Prelude


import           Data.Text (Text)
import           Text.Printf    
import qualified Data.Text as T    
--import           Data.Geo.Geodetic
import           Data.Geo.Math
import           Data.Geo.TransverseMercator


    
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

tile :: (GeoFloat f) => Length f                    
tile = 100 *~ kilo meter

upseasting, utmeasting, maxutmSrow, minutmNrow ::
    (GeoFloat f) => Dimensionless f       
upseasting = 20 *~ one
utmeasting = 5 *~ one
maxutmSrow = 100 *~ one
minutmNrow = 0 *~ one

falseeasting :: GeoFloat f => [Length f]             
falseeasting = 
  [  upseasting * tile,
     upseasting * tile,
     utmeasting * tile,
     utmeasting * tile
  ]
  
falsenorthing :: GeoFloat f => [Length f]
falsenorthing =
  [ upseasting * tile, upseasting * tile
  , maxutmSrow * tile, minutmNrow * tile ]

wgs84semiMajor :: (GeoFloat f) => Length f
wgs84semiMajor = 6378137.0 *~ meter
wgs84flattening :: (GeoFloat f) => Dimensionless f                 
wgs84flattening = (3.3528106647474805e-3) *~ one

utmTransverseMercator :: (GeoFloat f) => TransverseMercator f
utmTransverseMercator =
    case (traverseMercator
          wgs84semiMajor
          wgs84flattening
          (0.9996 *~ one)
          tmAlphaBeta6) of
      Just tm -> tm
      Nothing -> error "utmTransverseMercator: unable to create kernel for wgs84"
                
utmTauf, utmTaupf :: GeoFloat f => PlaneAngle f -> PlaneAngle f
utmTauf = tauf utmTransverseMercator
utmTaupf = taupf utmTransverseMercator          

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

  
utmForward' :: GeoFloat f => PlaneAngle f -> PlaneAngle f ->
              Maybe ((Int, Bool), TM f)
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
           then tmForward utmTransverseMercator lon0 lat lon
           else error $ "utmForward: UPS not implemented yet"
      ind :: Int
      ind = (P.+) (if utmp then 2 else 0) (if northp1 then 1 else 0)
  in if (zone1 == zoneSpec' INVALID) then Nothing
     else Just $ ((zone1, northp1),
                       tm { _easting = _easting tm + (falseeasting !! ind)
                          , _northing = _northing tm + (falsenorthing !! ind)
                          })

utmForward :: GeoFloat f =>
  PlaneAngle f
  -> PlaneAngle f
  -> Maybe ((Int, Bool), (Length f, Length f))
utmForward lat lon = do
   (zonenorthp, tm) <- utmForward' lat lon
   return (zonenorthp, (_easting tm, _northing tm))                 
                        
utmReverse' :: (GeoFloat f) => Int -> Bool -> Length f -> Length f -> Maybe (TMR f)
utmReverse' zone northp x y
    | (isNanD meter x) || (isNanD meter y) || zone == zoneSpec' INVALID =
        Nothing
    | otherwise =
        let aa = 'a'
            utmp = zone /= zoneSpec' UPS
            ind = (P.+) (if utmp then 2 else 0) (if northp then 1 else 0)
            _x = x - (falseeasting !! ind)
            _y = y - (falsenorthing !! ind)
            lon0 = centralMeridian zone
        in if (utmp)
           then Just $ tmReverse utmTransverseMercator lon0 _x _y
           else error "utmReverse: UPS not implemented yet"

                
encodeZone :: Int -> Bool -> Bool -> Maybe Text
encodeZone zone northp abbrev
    | zone == zoneSpec' INVALID = Nothing
    | (not $ (zone >= zoneSpec' MINZONE) &&  (zone <= zoneSpec' MAXZONE))
        = Nothing
    | otherwise =
       let z = if (zone /= zoneSpec' UPS)
               then printf "%02d" zone
               else ""
           ns = if (abbrev)
                then if northp then "n" else "s"
                else if northp then "north" else "south"
       in Just $ T.pack $ z ++ ns

{-    
_lat_,_lon_ :: PlaneAngle Double
_lat_ = (49 *~ degree) + (29 *~ arcminute) + (13.6 *~ arcsecond)
_lon_ = (8 *~ degree) + (27 *~ arcminute) + (58.6 *~ arcsecond)

--    49° 29′ 13.6″ N
--    8° 27′ 58.6″ E

t = utmForward' _lat_ _lon_
u = let (tz, tn, t') = maybe (error "t") id t
    in utmReverse' tz tn (_easting t') (_northing t')



--f a b = ( a *~ degree) ( b *~ degree)        

--f :: Double -> [(PlaneAngle Double, PlaneAngle Double)]
f a' =
    let vs =zipWith (utmForward)
             (repeat $ a' *~ degree)
             [ (4 *~ degree) * (ll *~ degree) | ll <- [(-19)..19]]

        vs' = fmap (snd . fromJust) vs
    in sa' vs'

aa :: Int -> IO ()       
aa i = T.writeFile ("/tmp/test" ++ show i ++ ".svg") g
  
    
g = T.concat [
    "<?xml version=\"1.0\" standalone=\"no\"?>\n",
    "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n", 
    "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n",
    "<svg width=\"800\" height=\"600\" viewBox=\"-40000000 -40000000 80000000 80000000\" ",
    "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n",
    T.concat $ fmap f [(P.*) i 10 | i <- [-17 .. 17]],
    "\n</svg>\n"
    ]
--sa :: (Length Double, Length Double) -> (Int, Int)    
sa' [] = T.empty       
sa' xs =
    let as = fmap (sa "L") $ drop 1 xs
        a = sa "M" $ head xs
        as' = T.concat $ a : as
    in T.concat ["<path d=\"", as',"\" stroke=\"red\" fill=\"white\" stroke-width=\"100000\" />"]


minlat,maxlat,minlon,maxlon :: PlaneAngle Double
minlat = (54.0 *~ degree) + (12 *~ arcminute)
maxlat = (54.0 *~ degree) + (22 *~ arcminute)
minlon = (13.0 *~ degree) + (23 *~ arcminute)
maxlon = (13.0) *~ degree + (46 *~ arcminute)


sss _x = 
    let (xmin,ymin) = snd . fromJust $ utmForward minlat minlon
        (xmax, ymax) = snd . fromJust $ utmForward maxlat maxlon
        (xl, yl) = (xmax - xmin, ymax - ymin)
        ar = (xl / yl) /~ one       
        y = round $ (P.*) _x ar
        x = round _x
        (_x0, _y0) = (xmin - 1 *~ kilo meter, ymin - 1*~ kilo meter)
        (x0, y0) = (round (_x0 /~ meter), round (_y0 /~ meter))
        (_w, _h) = (xl + 2 *~ kilo meter, yl + 2 *~ kilo meter)
        (w,h) = (round (_w /~ meter), round (_h /~ meter))
    in T.pack $ concat ["<svg width=\"", show x, "\" height=\"", show y
                       ,"\" viewbox=\"", show x0, " ", show y0, " "
                       , show w, " ", show h, "\" "
                       , "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"]
       
foo =
    let minlat,maxlat,minlon,maxlon,steplat,steplon :: PlaneAngle Double
        minlat = (54.0 *~ degree) + (12 *~ arcminute)
        maxlat = (54.0 *~ degree) + (22 *~ arcminute)
        minlon = (13.0 *~ degree) + (23 *~ arcminute)
        maxlon = (13.0) *~ degree + (46 *~ arcminute)
        steplat = 1.0 *~ arcminute
        steplon = 3.0 *~ arcminute
        nlat,nlon :: Int
        nlat = round (((maxlat - minlat) / steplat) /~ one)
        nlon = round (((maxlon - minlon) / steplon) /~ one)
        latsteps = [ minlat + (fromIntegral i *~ one) * steplat  | i <- [0..nlat]]
        lonsteps = [ minlon + (fromIntegral i *~ one) * steplon  | i <- [0..nlon]]
    in (latsteps, lonsteps)

x = fmap (\l -> l /~ degree) $ snd foo
       
bar =
    let (latsteps,lonsteps) = foo
        ff lat = fmap (\lon -> (lat, lon)) lonsteps
        gg lon = fmap (\lat -> (lat, lon)) latsteps
    in (fmap ff lonsteps, fmap gg latsteps)


sa p (xd, yd) =
    let x,y :: Int
        (x,y) = ((round (xd /~ meter)), (round (yd /~ meter)))
    in T.pack $ concat [p, show x, " ", show y ]

sa' [] = T.empty       
sa' xs =
    let as = fmap (sa "L") $ drop 1 xs
        a = sa "M" $ head xs
        as' = T.concat $ a : as
    in T.concat ["<path d=\"", as',"\" stroke=\"red\" fill=\"white\" stroke-width=\"200\" />"]


       

f =
    let a = fmap (fmap (\(lat,lon) ->
                            let as = fromJust $ utmForward lat lon
                            in snd as )) $ fst bar
        b = fmap (fmap (\(lat,lon) ->
                            let as = fromJust $ utmForward lat lon
                            in snd as )) $ snd bar
                            
    in fmap sa' b 


aa :: Int -> IO ()       
aa i = T.writeFile ("/tmp/test" ++ show i ++ ".svg") g
  
    
g = T.concat [
    "<?xml version=\"1.0\" standalone=\"no\"?>\n",
    "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n", 
    "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n",
    sss 1000,                                                             
    ">\n",
    T.concat f,
    "\n</svg>\n"
    ]       

{-
1960712 1525217
2005126 1527711
        
  44414    2494  



3795558 7447648
3787954 7470832

   7604   23184


1834 846
-}
-}
