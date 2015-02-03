module Data.Geo.Math where

import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
    


class (RealFloat f) => GeoFloat f where
  infinity :: Dimensionless f
  negInfinity :: Dimensionless  f
  negInfinity = (infinity * ((-1) *~ one)) 
  epsilon :: Dimensionless f
  overflow :: Dimensionless f
  overflow = _1 / (epsilon * epsilon)
  tol :: Dimensionless f
  tol = (0.1 *~ one) * sqrt(epsilon)
  tanx :: Dimensionless f -> Dimensionless f
  tanx x =
    let t = tan x
    in if (x >= _0)
       then if (t >= _0) then infinity else t
       else if (t < _0) then negInfinity else t

instance GeoFloat Double where
  infinity = (read "Infinity") *~ one
  epsilon = (_2 ** ((-53) *~ one))


--fromIntegralDim :: (Integral a, Num b) => Unit d a -> Quantity d a -> Quantity d b



--b = fromIntegralDim one one

isFinite :: RealFloat a => Unit d a -> Quantity d a -> Bool
isFinite u x' =
  let x = x' /~ u
  in not $ isNaN x || isInfinite x


hypot :: (Ord t, Floating t) => Dimensionless t -> Dimensionless t -> Dimensionless t
hypot x' y' =
  let _x = abs x'
      _y = abs y'
      (x, __y) = if (_x < _y) then (_y, _x) else (_x, _y)
      ty = if (x == _0) then _1 else x
      y = __y / ty
  in x * (sqrt(_1 + y * y))

sumE :: Num t => Quantity q t -> Quantity q t -> (Quantity q t, Quantity q t)
sumE u v =
  let s = u + v
      _up = s - v
      _vpp = s - up
      up = _up - u
      vpp = _vpp - v
      t = negate (up + vpp)
  in (s,t)
      

angNormalize :: (Ord a, Floating a) => PlaneAngle a -> PlaneAngle a
angNormalize x =
  if (x >= (180 *~ degree)) then x - (360 *~ degree)
  else if (x < (-180) *~ degree) then x + (360 *~ degree) else x


angDiff :: (Ord a, Floating a) => PlaneAngle a -> PlaneAngle a -> PlaneAngle a
angDiff x y =
  let (_d, t) = sumE (negate x) y
      d = if ((_d - (180 *~ degree)) + t > _0)
          then _d - (360 *~ degree)
          else if ((_d + (180 *~ degree)) + t <= _0)
               then _d + (360 *~ degree) else _d
  in d + t


