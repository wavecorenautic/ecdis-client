module Data.Geo.Math where
       
class (RealFloat f) => GeoFloat f where
  infinity :: f
  negInfinity :: f
  negInfinity = infinity * (-1)
  epsilon :: f
  overflow :: f
  overflow = 1 / (epsilon * epsilon)
  tol :: f
  tol = 0.1 * sqrt(epsilon)
  tanx :: f -> f
  tanx x =
    let t = tan x
    in if (x >= 0)
       then if (t >= 0) then infinity else t
       else if (t < 0) then negInfinity else t

instance GeoFloat Double where
  infinity = (read "Infinity")
  epsilon = 2 ** (-53)

isFinite :: RealFloat a => a -> Bool
isFinite x = not $ isNaN x || isInfinite x


hypot :: (Ord a, Floating a) => a -> a -> a
hypot x' y' =
  let _x = abs x'
      _y = abs y'
      (x, __y) = if (_x < _y) then (_y, _x) else (_x, _y)
      ty = if (x == 0) then 1 else x
      y = __y / ty
  in x * (sqrt(1 + y * y))

sumE :: Num t => t -> t -> (t, t)
sumE u v =
  let s = u + v
      _up = s - v
      _vpp = s - up
      up = _up - u
      vpp = _vpp - v
      t = -(up + vpp)
  in (s,t)
      

angNormalize :: (Ord a, Num a) => a -> a
angNormalize x =
  if (x >= 180) then x - 360
  else if (x < (-180)) then x + 360 else x
                                         
angDiff :: (Ord a, Num a) => a -> a -> a
angDiff x y =
  let (_d, t) = sumE (-x) y
      d = if ((_d - 180) + t > 0)
          then _d - 360
          else if ((_d + 180) + t <= 0)
               then _d + 360 else _d
  in d + t



