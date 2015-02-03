{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Geo.UTM where


import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude

import           Control.Lens ((^.))
import           Data.Geo.Geodetic
import           Data.Geo.TraverseMercator

    
utmTraverseMercator :: TraverseMercator Double
utmTraverseMercator =
    case (traverseMercator
          ((wgs84 ^. _SemiMajor) *~ meter)
          ((wgs84 ^. _Flattening) *~ one)
          (0.9996 *~ one)
          tmAlphaBeta6) of
      Just tm -> tm
      Nothing -> error "utmTraverseMercator: unable to create kernel for wgs84"
                
--utmForward :: PlaneAngle Double -> PlaneAngle Double -> Double -> TM Double
utmForward = tmForward utmTraverseMercator

--utmTauf, utmTaupf :: PlaneAngle Double -> PlaneAngle Double             
utmTauf = tauf utmTraverseMercator
utmTaupf = taupf utmTraverseMercator          
             


t = utmForward (9 *~ degree) (52 *~ degree) (7.5 *~ degree)


{-                      n = 5762100.5
        e = 397027
        z = "32N"
    in ((x,y), ((n - x, e -y)))
       

-}

--




