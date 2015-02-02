{-# LANGUAGE FlexibleContexts #-}

module Data.Geo.TraverseMercator
       ( TraverseMercator (..)
       , traverseMercator
       , alphaBeta6
       , tmForward
       , tauf
       ) where

import           Control.Applicative
import           Control.Lens
import           Data.Geo.Geodetic
import           Data.Radian


class (RealFloat f) => GeoFloat f where
  infinity :: f
  negInfinity :: f
  negInfinity = infinity * (-1)
  epsilon :: f
  overflow :: f
  overflow = 1 / (epsilon * epsilon)
  tol :: f
  tol = 0.1 * sqrt(epsilon)

instance GeoFloat Double where
  infinity = (read "Infinity")
  epsilon = 2 ** (-53)

isFinite :: RealFloat a => a -> Bool
isFinite x = not $ isNaN x || isInfinite x

  
data TraverseMercator t =
    TraverseMercator {
      _TMf :: t,
      _TMalps :: [t],
      _TMbets :: [t],
      _TMa :: t,
      _TMn :: t,
      _TMe2 :: t,
      _TMe :: t,
      _TMe2m :: t,
      _TMc :: t,
      _TMa1 :: t,
      _TMb1 :: t,
      _TMk0 :: t
    } deriving (Show, Eq)

tanx :: GeoFloat f => f -> f
tanx x =
  let t = tan x
  in if (x >= 0)
     then if (t >= 0) then infinity else t
     else if (t < 0) then negInfinity else t

eatanhe :: (Ord s, Floating s) => TraverseMercator s -> s -> s
eatanhe tm = eatanhe' (_TMf tm) (_TMe tm)

eatanhe' :: (Ord a, Num a, Floating s) => a -> s -> s -> s
eatanhe' _f _e x
  | (_f >= 0) = _e * (atanh $ _e * x)
  | otherwise = _e * (atan $ _e * x)

hypot :: (Ord a, Floating a) => a -> a -> a
hypot x' y' =
  let _x = abs x'
      _y = abs y'
      (x, __y) = if (_x < _y) then (_y, _x) else (_x, _y)
      ty = if (x == 0) then 1 else x
      y = __y / ty
  in x * (sqrt(1 + y * y))
      
taupf :: GeoFloat f => TraverseMercator f -> f ->  f
taupf tm tau =
  let tau1 = hypot 1 tau
      sig  = sinh . eatanhe tm $ tau / tau1
  in if (not $ (abs tau) < overflow) then tau
     else (hypot 1 sig) * tau - sig * tau1


tauf :: GeoFloat f => TraverseMercator f -> f -> f
tauf tm taup =
  let _tau = taup / _TMe2m tm
      _stol = tol * (max 1 $ abs taup)
      _e2m = _TMe2m tm
      numit = (length $ _TMalps tm) - 1
      tauL i tau =
        let tau1 = hypot 1 tau
            sig = sinh . eatanhe tm $ tau / tau1
            taupa = (hypot 1 sig) * tau - sig * tau1
            dtau = (taup -taupa) * (1 + (_e2m) * (tau * tau)) /
                   (_e2m * tau1 * (hypot 1 taupa))
            tau' = tau + dtau            
        in if ((abs dtau) < _stol || i >= numit) then tau'
           else tauL (i + (1 :: Int)) tau'
  in if (not $ (abs taup) < overflow) then taup
     else tauL 0 _tau

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

tmForward :: GeoFloat f => TraverseMercator f -> f -> f -> f -> ((f, f), f, f)  
tmForward tm lon0' lat' lon' =
  let cs@(lat, _, lon, _, _) =
        tmNormalizeInput lon0' lat' lon'
      params@(etap, xip, _, _) =
        tmParameters tm lat lon
      c0  = cos  $ 2 * xip
      ch0 = cosh $ 2 * etap
      s0  = sin  $ 2 * xip
      sh0 = sinh $ 2 * etap
      ar = 2 * c0 * ch0
      ai = (-2) * s0 * sh0
      alp = _TMalps tm
      mp = (length alp) - 1             
      startp = if (odd mp) then mp - 1 else mp                          
      mat0 = ( if (odd mp) then alp !! mp else 0, 0
             , 0, 0
             , if (odd mp) then 2 * (fromIntegral mp) * (alp !! mp) else 0, 0
             , 0, 0 )
  in tmFinal tm cs params s0 sh0 c0 ch0 .
     tmEvalMatrix ai ar alp startp $ mat0
  
tmNormalizeInput :: (Ord t, Num t) => t -> t -> t -> (t, t, t, t, Bool)
tmNormalizeInput lon0' lat' lon' =
  let _lon = (angNormalize lon0') `angDiff` (angNormalize lon')
      _latsign = signum lat'
      _lonsign = signum _lon
      lat = lat' * _latsign
      __lon = _lon * _lonsign      
      backside = __lon > 90
      lonsign = _lonsign
      (latsign, lon) =
        if backside
        then if (lat == 0) then (-1, 180 - __lon) else (_latsign, __lon)
        else (_latsign, __lon)
  in (lat, latsign, lon, lonsign, backside)

tmParameters :: GeoFloat f => TraverseMercator f -> f -> f -> (f, f, f, f)
tmParameters tm lat lon =
  let e2m = _TMe2m tm
      e2  = _TMe2 tm
      phi = lat ^. toRadians
      lam = lon ^. toRadians
  in if (lat == 90)
     then (0, pi / 2, lam, _TMc tm)
     else
       let c = max 0 lam
           tau = tan phi
           taup = taupf tm tau
       in (asinh $ (sin lam) / (hypot taup c) -- etap
          , atan2 taup c -- xip
          , atan $ (tanx lam) * taup / (hypot 1 taup)   -- gamma
          , (sqrt $ e2m + e2 * (cos phi * cos phi)) / (hypot taup c) -- k
          )            


tmEvalMatrix :: Fractional t => t
                -> t
                -> [t]
                -> Int
                -> (t, t, t, t, t, t, t, t)
                -> (t, t, (t, t, t, t, t, t, t, t))
tmEvalMatrix ar ai _alp n params =
  (ar / 2, ai / 2, tmEvalMatrix' ar ai _alp n params)

tmEvalMatrix' :: Num t =>
                 t
                 -> t
                 -> [t]
                 -> Int
                 -> (t, t, t, t, t, t, t, t)
                 -> (t, t, t, t, t, t, t, t)                 
tmEvalMatrix'
  ar ai _alp n (_xi0, _xi1, _eta0, _eta1, _yr0, _yr1, _yi0, _yi1) =
    let xi1  = ar * _xi0 - ai * _eta0 - _xi1 + (_alp !! n)
        eta1 = ai * _xi0 + ar * _eta0 - _eta1
        yr1  = ar * _yr0 - ai * _yi0 - _yr1 + 2 * (fromIntegral n) * (_alp !! n)
        yi1  = ai * _yr0 + ar * _yi0 - _yi1
            
        n' = n - 1
        xi0 = ar * xi1 - ai * eta1 - _xi0 + (_alp !! n')
        eta0 = ai * xi1 + ar * eta1 - _eta0
        yr0 = ar * yr1 - ai * yi1 - _yr0 +
              2 * (fromIntegral n') * (_alp !! n')
        yi0 = ai * yr1 + ar * yi1 - _yi0
        res = (xi0, xi1, eta0, eta1, yr0, yr1, yi0, yi1)
        n'' = n' - 1
    in if (n'' <= 0)
       then res
       else tmEvalMatrix' ar ai _alp (n'') res

tmFinal :: RealFloat t =>
           TraverseMercator t
           -> (t, t, t1, t, Bool)
           -> (t, t, t, t)
           -> t
           -> t
           -> t
           -> t
           -> (t, t, (t, t, t, t, t, t, t, t))
           -> ((t, t), t, t)
tmFinal tm 
  (_, latsign, _, lonsign, backside) (etap, xip, gamma_, k_) s0 sh0 c0 ch0
  (ar_, ai_, (xi0_, _, eta0_, _, yr0_, yr1_, yi0_, yi1_)) =
  let yr1 = 1 - yr1_ + ar_ * yr0_ - ai_ * yi0_
      yi1 =   - yi1_ + ai_ * yr0_ + ar_ * yi0_
      ar = s0 * ch0
      ai = c0 * sh0
      xi  = xip  + ar * xi0_ - ai * eta0_
      eta = etap + ai * xi0_ + ar * eta0_
      _gamma = ((gamma_ - (atan2 yr1 yi1)) ^. fromRadians) * latsign * lonsign
      gamma = if (backside) then 180 - _gamma
              else _gamma
      _k = k_ * (_TMb1 tm * (hypot yr1 yi1))
      k = _k * (_TMk0 tm)
      y = (_TMa1 tm) * (_TMk0 tm) * latsign * (if backside then pi - xi else xi)
      x = (_TMa1 tm) * (_TMk0 tm) * eta * lonsign
  in ((x,y),gamma,k)



traverseMercator :: 
  (AsFlattening (->) (Const Double) e,
   AsSemiMajor (->) (Const Double) e
  ) =>
  e
  -> Double
  -> (Double -> (Double, [Double], [Double]))
  -> Maybe (TraverseMercator Double)
traverseMercator e _k0 mkAlpBet =
    let _a = semiMajorAxis e
        __f = flattening e
        _f = if (__f < 1) then __f else 1 / __f
        _e2 = _f * (2 - _f)
        _e = sqrt(abs _e2)
        _e2m = 1 - _e2
        _eatanhe = eatanhe' _f _e
        _c = sqrt(_e2m) * exp(_eatanhe 1.0)
        _n = _f / (2 - _f)             
        (_b1, _alps,_bets) = mkAlpBet _n
        _a1 = _b1 * _a
        tm = TraverseMercator {
               _TMalps = _alps,
               _TMbets = _bets,
               _TMf = _f,
               _TMa = _a,
               _TMn = _n,
               _TMe = _e,
               _TMe2 = _e2,
               _TMe2m = _e2m,
               _TMc = _c,
               _TMa1 = _a1,
               _TMb1 = _b1,
               _TMk0 = _k0
             }
        pa = not (isFinite _a && _a > 0)
        pb = not (isFinite _f && _f < 1)
        pc = not (isFinite _k0 && _k0 > 0) 
        in if (pa || pb || pc) then Nothing
           else Just tm

alphaBeta6 :: Fractional t => t -> (t, [t], [t])
alphaBeta6 _n =
    let nx = _n * _n
        _b1 = 1/(1+_n)*(nx*(nx*(nx+4)+64)+256)/256;
        _alp1 = _n*(_n*(_n*(_n*(_n*(31564*_n-66675)+34440)+47250)-100800)+
                           75600)/151200;
        _bet1 = _n*(_n*(_n*(_n*(_n*(384796*_n-382725)-6720)+932400)-1612800)+
                    1209600)/2419200;
        _alp2 = nx*(_n*(_n*((863232-1983433*_n)*_n+748608)-1161216)+524160)/
                  1935360;
        _bet2 = nx*(_n*(_n*((1695744-1118711*_n)*_n-1174656)+258048)+80640)/
                  3870720;
                         
        nx2 = nx * _n              
        _alp3 = nx2*(_n*(_n*(670412*_n+406647)-533952)+184464)/725760;
        _bet3 = nx2*(_n*(_n*(22276*_n-16929)-15984)+12852)/362880;
                  
        nx3 = nx2 * _n
        _alp4 = nx3*(_n*(6601661*_n-7732800)+2230245)/7257600;
        _bet4 = nx3*((-830251*_n-158400)*_n+197865)/7257600;
                  
        nx4 = nx3 * _n
        _alp5 = (3438171-13675556*_n)*nx4/7983360;
        _bet5 = (453717-435388*_n)*nx4/15966720;
        nx5 = nx4 * _n
        _alp6 = 212378941*nx5/319334400;
        _bet6 = 20648693*nx5/638668800;
    in ( _b1
       , [_alp1,_alp2,_alp3,_alp4,_alp5,_alp6]
       , [_bet1,_bet2,_bet3,_bet4,_bet5,_bet6]
       )



semiMajorAxis :: AsSemiMajor (->) (Const Double) t => t -> Double
semiMajorAxis e = view _SemiMajor e

flattening :: AsFlattening (->) (Const Double) t => t -> Double
flattening e = view _Flattening e

