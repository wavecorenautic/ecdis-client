{-# LANGUAGE FlexibleContexts #-}

module Data.Geo.TransverseMercator
       ( TransverseMercator (..)       
       , traverseMercator
       , tmAlphaBeta6
       , TM (..)
       , TMR (..)
       , tmForward
       , tmReverse
       , tauf
       , taupf
       ) where


import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
    

import           Data.Geo.Math
import           Data.Geo.AlphaBeta
    
data TransverseMercator t =
    TransverseMercator {
      _TMf :: Dimensionless t,
      _TMalps :: ! [Dimensionless t],
      _TMbets :: ! [Dimensionless t],
      _TMa :: Length t,
      _TMn :: Dimensionless t,
      _TMe2 :: Dimensionless t,
      _TMe :: Dimensionless t,
      _TMe2m :: Dimensionless t,
      _TMc :: Dimensionless t,
      _TMa1 :: Length t,
      _TMb1 :: Dimensionless t,
      _TMk0 :: Dimensionless t
    } deriving (Show, Eq)

data TM t = TM {
  _easting :: Length t, -- meter
  _northing :: Length t, -- meter
  _meridianConvergence :: PlaneAngle t, -- degree
  _projectionScale :: Dimensionless t -- unity
  } deriving (Show, Eq)

data TMR t = TMR {
  _rLatitude :: PlaneAngle t,
  _rLongitude :: PlaneAngle t,
  _rMeridianConvergence :: PlaneAngle t, -- degree
  _rPojectionScale :: Dimensionless t -- unity
  } deriving (Show, Eq)

traverseMercator :: RealFloat a =>
                   Length a -> Dimensionless a -> Dimensionless a ->
                   (Dimensionless a ->
                        (Dimensionless a, [Dimensionless a], [Dimensionless a]))
                       -> Maybe (TransverseMercator a)
traverseMercator _a __f _k0 mkAlpBet =
    let 
        _f = if (__f < _1) then __f else _1 / __f
        _e2 = _f * (_2 - _f)
        _e = sqrt(abs _e2)
        _e2m = _1 - _e2
        _eatanhe = eatanhe' _f _e
        _c = sqrt(_e2m) * exp(_eatanhe _1)
        _n = _f / (_2 - _f)             
        (_b1, _alps,_bets) = mkAlpBet _n
        _a1 = _b1 * _a
        tm = TransverseMercator {
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
        pa = not (isFinite meter _a && _a > _0)
        pb = not (isFinite one _f && _f < _1)
        pc = not (isFinite one _k0 && _k0 > _0) 
        in if (pa || pb || pc) then Nothing
           else Just tm

            
eatanhe :: (Ord s, Floating s) => TransverseMercator s -> PlaneAngle s -> PlaneAngle s
eatanhe tm = eatanhe' (_TMf tm) (_TMe tm)

eatanhe' :: (Ord a, Num a, Floating s) => Dimensionless a -> PlaneAngle s -> PlaneAngle s -> PlaneAngle s
eatanhe' _f _e x
  | (_f >= _0) = _e * (atanh $ _e * x)
  | otherwise = _e * (atan $ _e * x)


taupf :: GeoFloat f => TransverseMercator f -> PlaneAngle f -> PlaneAngle f
taupf tm _tau =
  let tau1 = hypot _1 _tau
      sig  = sinh . eatanhe tm $ _tau / tau1
  in if (not $ (abs _tau) < overflow) then _tau
     else (hypot _1 sig) * _tau - sig * tau1


tauf :: GeoFloat f => TransverseMercator f -> PlaneAngle f -> PlaneAngle f
tauf tm taup =
  let _tau = taup / _e2m
      _stol = tol * (max _1 $ abs taup)
      _e2m = _TMe2m tm
      numit = 5 :: Int  
      tauL i _tau =
        let tau1 = hypot _1 _tau
            sig = sinh . eatanhe tm $ _tau / tau1
            taupa = (hypot _1 sig) * _tau - sig * tau1
            dtau = (taup - taupa) * (_1 + _e2m * (tau * tau)) /
                   (_e2m * tau1 * (hypot _1 taupa))
            tau' = _tau + dtau            
        in if ((abs dtau) < _stol || i >= numit) then tau'
           else tauL (succ i) tau'
  in if (not $ (abs taup) < overflow) then taup
     else tauL 0 _tau


tmReverse :: GeoFloat f => TransverseMercator f ->
            PlaneAngle f -> Length f -> Length f -> TMR f
tmReverse tm lon0 x y =
  let _a1 = _TMa1 tm
      _k0 = _TMk0 tm
      sf = (_a1 * _k0)
      __xi = y / sf
      _eta = x / sf
      xisign = (signum $ xi /~ one) *~ one
      etasign = (signum $ eta /~ one) *~ one
      _xi = __xi * xisign
      eta = _eta * etasign
      backside = _xi > (pi / _2)
      xi = if backside then pi - _xi else _xi
      c0 = cos $ _2 * xi
      ch0 = cosh $ _2 * eta
      s0 = sin $ _2 * xi
      sh0 = sinh $ _2 * eta
      ar = _2 * c0 * ch0
      ai = _2 * s0 * sh0
      _bet = _TMbets tm
      maxpow = pred $ length _bet
      startp = if (odd maxpow) then (pred maxpow) else maxpow
      mat0 = (if (odd maxpow) then (_0 - (_bet !! 0)) else _0, _0 -- xip0/1
              , _0, _0  -- etap0/1
              , if (odd maxpow)
                then _0 - _2 * (fromIntegral maxpow *~ one) * (_bet !! maxpow)
                else _0, _0 -- yr0/1
              , _0, _0) -- yi0/1
      mat1 = tmEvalMatrix ai ar _bet startp mat0
      (lam', phi', gamma', k') = tmRvFinalize tm xi eta s0 c0 sh0 ch0 mat1
      lat = phi' * xisign
      _lon = if backside
             then ((180 *~ degree) - lam') * etasign
             else lam' * etasign
      lon = angNormalize $ _lon + angNormalize lon0
      gamma = if backside then ((180 *~ degree) - gamma') * xisign
              else gamma' * xisign
      k = k' * (_TMk0 tm)
  in TMR {
     _rLatitude  = lat,
     _rLongitude = lon,
     _rMeridianConvergence = gamma,
     _rPojectionScale = k
     }

tmRvFinalize ::   (GeoFloat a) => TransverseMercator a
  -> Dimensionless a -> Dimensionless a -> Dimensionless a -> Dimensionless a
  -> Dimensionless a -> Dimensionless a
  -> (Dimensionless a, Dimensionless a,
      (Dimensionless a, Dimensionless a, Dimensionless a, Dimensionless a,
       Dimensionless a, Dimensionless a, Dimensionless a, Dimensionless a))
  -> (Dimensionless a, Dimensionless a, Dimensionless a, Dimensionless a )      
tmRvFinalize tm xi eta s0 c0 sh0 ch0
  (ar_, ai_, (xip0_, _, etap0_, _, yr0_, yr1_, yi0_, yi1_)) =
  let yr1 = _1 - yr1_ + ar_ * yr0_ - ai_ * yi0_
      yi1 = _0 - yi1_ + ai_ * yr0_ + ar_ * yi0_
      ar = s0 * ch0
      ai = c0 * sh0
      xip  = xi  + ar * xip0_ - ai * etap0_
      etap = eta + ai * xip0_ + ar * etap0_
      gamma = atan2 yi1 yr1
      k = (_TMb1 tm) / (hypot yr1 yi1)
      s = sinh etap
      c = max _0 $ cos xip
      r = hypot s c
  in if (r /= _0)
     then let _lam = atan2 s c
              _taup = (sin xip) / r
              _tau = tauf tm _taup
              _phi = atan _tau
              _gamma = gamma + (atan $ tanx xip * tanh etap)
              _k = k * (sqrt $ (_TMe2m tm) + (_TMe2 tm) * (cos _phi * cos _phi))
                   * (hypot _1 _tau) * r
          in (_lam, _phi, _gamma, _k)
     else (_0, pi / _2, gamma, k)


tmForward :: GeoFloat f => TransverseMercator f ->
            PlaneAngle f -> PlaneAngle f -> PlaneAngle f -> TM f
tmForward tm lon0' lat' lon' =
  let cs@(lat, _, lon, _, _) =
        tmFwNormalizeInput lon0' lat' lon'
      params@(etap, xip, _, _) =
        tmFwParameters tm lat lon
      c0  = cos  $ _2 * xip
      ch0 = cosh $ _2 * etap
      s0  = sin  $ _2 * xip
      sh0 = sinh $ _2 * etap
      ar = _2 * c0 * ch0
      ai = ((-2) *~ one) * s0 * sh0
      alp = _TMalps tm
      maxpow = pred $ length alp 
      startp = if (odd maxpow) then (pred maxpow) else maxpow
      -- (xi0, xi1, eta0, eta1, yr0, yr1, yi0, yi1)
      mat0 = ( if (odd maxpow) then (alp !! maxpow) else _0, _0
             , _0, _0
             , if (odd maxpow)
               then _2 * ((fromIntegral maxpow) *~ one) * (alp !! maxpow)
               else _0, _0, _0, _0 )          
  in tmFwFinal tm cs params s0 sh0 c0 ch0 .
     tmEvalMatrix ai ar alp startp $ mat0


tmFwNormalizeInput :: (Ord t, Floating t) => PlaneAngle t -> PlaneAngle t ->
                     PlaneAngle t -> (PlaneAngle t, PlaneAngle t, Dimensionless t,
                                                Dimensionless t, Bool)

tmFwNormalizeInput lon0' lat' lon' =
  let _lon = (angNormalize lon0') `angDiff` (angNormalize lon')
      _latsign = signum (lat' /~ degree) *~ one
      lonsign = signum (_lon /~ degree) *~ one
      lat = lat' * _latsign
      __lon = _lon * lonsign      
      backside = __lon > (90 *~ degree)
      (latsign, lon) =
        if backside
        then
          let l = (180 *~ degree) - __lon
          in if (lat == _0) then ((-1) *~ one, l) else (_latsign, l)
        else (_latsign, __lon)
  in (lat, latsign, lon, lonsign, backside)

tmFwParameters :: GeoFloat f => TransverseMercator f -> PlaneAngle f -> PlaneAngle f -> (PlaneAngle f, PlaneAngle f, PlaneAngle f, PlaneAngle f)
tmFwParameters tm lat lon =
  let e2m = _TMe2m tm
      e2  = _TMe2 tm
      phi = lat 
      lam = lon 
  in if (lat == (90 *~ degree))
     then (_0, pi / _2, lam, _TMc tm)
     else
       let c = max _0 (cos lam)
           _tau = tan phi
           taup = taupf tm _tau
       in (asinh $ (sin lam) / (hypot taup c) -- etap
          , atan2 taup c -- xip
          , atan $ (tanx lam) * taup / (hypot _1 taup)   -- gamma
          , sqrt $ e2m + e2 * (cos phi * cos phi) / (hypot taup c) -- k
          )            


tmEvalMatrix :: Fractional t => Dimensionless t
                -> Dimensionless t
                -> [Dimensionless t]
                -> Int
                -> (Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t)
                -> (Dimensionless t, Dimensionless t, (Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t))

tmEvalMatrix ar ai _coeffs n params =
  (ar / _2, ai / _2, tmEvalMatrix' ar ai _coeffs n params)


tmEvalMatrix' :: Num t =>
                 Dimensionless t
                 -> Dimensionless t
                 -> [Dimensionless t]
                 -> Int
                 -> (Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t)
                 -> (Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t)

 
tmEvalMatrix'
  ar ai coeffs n (_xi0, _xi1, _eta0, _eta1, _yr0, _yr1, _yi0, _yi1) =
    let xi1  = ar * _xi0 - ai * _eta0 - _xi1 + (coeffs !! (n))
        eta1 = ai * _xi0 + ar * _eta0 - _eta1
        yr1  = ar * _yr0 - ai * _yi0 - _yr1 + _2 * (fromIntegral n *~ one) * (coeffs !! (n))
        yi1  = ai * _yr0 + ar * _yi0 - _yi1
            
        n' = pred n
        xi0 = ar * xi1 - ai * eta1 - _xi0 + (coeffs !! (n'))
        eta0 = ai * xi1 + ar * eta1 - _eta0
        yr0 = ar * yr1 - ai * yi1 - _yr0 +
              _2 * (fromIntegral n' *~ one) * (coeffs !! (n'))
        yi0 = ai * yr1 + ar * yi1 - _yi0
        res = (xi0, xi1, eta0, eta1, yr0, yr1, yi0, yi1)
        n'' = pred n'
    in if (n'' <= 0)
       then res
       else tmEvalMatrix' ar ai coeffs (n'') res



tmFwFinal :: RealFloat t =>
           TransverseMercator t
           -> (Dimensionless t, Dimensionless t, Dimensionless t1, Dimensionless t, Bool)
           -> (Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t)
           -> Dimensionless t
           -> Dimensionless t
           -> Dimensionless t
           -> Dimensionless t
           -> (Dimensionless t, Dimensionless t, (Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t, Dimensionless t))
           -> TM t
tmFwFinal tm 
  (_, latsign, _, lonsign, backside) (etap, xip, gamma_, k_) s0 sh0 c0 ch0
  (ar_, ai_, (xi0_, _, eta0_, _, yr0_, yr1_, yi0_, yi1_)) =
  let yr1 = _1 - yr1_ + ar_ * yr0_ - ai_ * yi0_
      yi1 =   (negate yi1_) + ai_ * yr0_ + ar_ * yi0_
      ar = s0 * ch0
      ai = c0 * sh0
      xi  = xip  + ar * xi0_ - ai * eta0_
      eta = etap + ai * xi0_ + ar * eta0_
      _gamma = gamma_ - (atan2 yi1 yr1)
      gamma = if (backside) then ((180 *~ degree) - _gamma) * latsign * lonsign
              else _gamma * latsign * lonsign
      _a1 = (_TMa1 tm)
      _k0 = (_TMk0 tm)
      _b1 = (_TMb1 tm)
      _k = k_ * _b1  * (hypot yr1 yi1)
      k = _k * _k0
      y = _a1 * _k0 * latsign * (if backside then pi - xi else xi)
      x = _a1 * _k0 * eta * lonsign
  in TM { _easting = x,
          _northing = y,
          _meridianConvergence = gamma,
          _projectionScale = k
        }

tmAlphaBeta6 :: Fractional a =>
               Dimensionless a -> (Dimensionless a, [Dimensionless a], [Dimensionless a])
tmAlphaBeta6 _n =
    let (a, as, bs) = _alphaBeta6 (_n /~ one)
        mkDim _a = _a *~ one
    in (mkDim a, fmap mkDim as, fmap mkDim bs)
