{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Geo.UTM where



import           Control.Lens
import           Data.Geo.Geodetic
import           Data.Geo.TraverseMercator

    
utmTraverseMercator :: TraverseMercator Double
utmTraverseMercator =
    case (traverseMercator (wgs84 ^. _SemiMajor) (wgs84 ^. _Flattening) 0.9996 alphaBeta6) of
      Just tm -> tm
      Nothing -> error "utmTraverseMercator: unable to create kernel for wgs84"
                
utmForward :: Double -> Double -> Double -> TM Double             
utmForward = tmForward utmTraverseMercator

utmTauf, utmTaupf :: Double -> Double             
utmTauf = tauf utmTraverseMercator
utmTaupf = taupf utmTraverseMercator          
             


t = utmForward 9 52 7.5


{-                      n = 5762100.5
        e = 397027
        z = "32N"
    in ((x,y), ((n - x, e -y)))
       

-}

--




