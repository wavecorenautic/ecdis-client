{-# LANGUAGE FlexibleContexts #-}

module Data.Geo.TraverseMercator
       ( TraverseMercator (..)       
       , traverseMercator
       , alphaBeta6
       , TM (..)
       , tmForward
       , tauf
       , taupf
       ) where

import           Control.Lens
import           Data.Radian
import           Data.Geo.Math
  
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

data TM t = TM {
  _easting :: t, -- meter
  _northing :: t, -- meter
  _meridianConvergence :: t, -- degree
  _projectionScale :: t -- unity
  } deriving (Show, Eq)


traverseMercator :: RealFloat a =>
                    a -> a -> a -> (a -> (a, [a], [a])) -> Maybe (TraverseMercator a)
traverseMercator _a __f _k0 mkAlpBet =
    let 
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


eatanhe :: (Ord s, Floating s) => TraverseMercator s -> s -> s
eatanhe tm = eatanhe' (_TMf tm) (_TMe tm)

eatanhe' :: (Ord a, Num a, Floating s) => a -> s -> s -> s
eatanhe' _f _e x
  | (_f >= 0) = _e * (atanh $ _e * x)
  | otherwise = _e * (atan $ _e * x)


taupf :: GeoFloat f => TraverseMercator f -> f ->  f
taupf tm tau =
  let tau1 = hypot 1 tau
      sig  = sinh . eatanhe tm $ tau / tau1
  in if (not $ (abs tau) < overflow) then tau
     else (hypot 1 sig) * tau - sig * tau1


tauf :: GeoFloat f => TraverseMercator f -> f -> f
tauf tm taup =
  let _tau = taup / _e2m
      _stol = tol * (max 1 $ abs taup)
      _e2m = _TMe2m tm
      numit = 5 :: Int
      tauL i tau =
        let tau1 = hypot 1 tau
            sig = sinh . eatanhe tm $ tau / tau1
            taupa = (hypot 1 sig) * tau - sig * tau1
            dtau = (taup - taupa) * (1 + _e2m * (tau * tau)) /
                   (_e2m * tau1 * (hypot 1 taupa))
            tau' = tau + dtau            
        in if ((abs dtau) < _stol || i >= numit) then tau'
           else tauL (succ i) tau'
  in if (not $ (abs taup) < overflow) then taup
     else tauL 0 _tau


tmForward :: GeoFloat f => TraverseMercator f -> f -> f -> f -> TM f
tmForward tm lon0' lat' lon' =
  let cs@(lat, _, lon, _, _) =
        tmFwNormalizeInput lon0' lat' lon'
      params@(etap, xip, _, _) =
        tmFwParameters tm lat lon
      c0  = cos  $ 2 * xip
      ch0 = cosh $ 2 * etap
      s0  = sin  $ 2 * xip
      sh0 = sinh $ 2 * etap
      ar = 2 * c0 * ch0
      ai = (-2) * s0 * sh0
      alp = _TMalps tm
      maxpow = (length alp) - 1             
      startp = if (odd maxpow) then maxpow - 1 else maxpow
      -- (xi0, xi1, eta0, eta1, yr0, yr1, yi0, yi1)
      mat0 = ( if (odd maxpow) then alp !! maxpow else 0, 0
             , 0, 0
             , if (odd maxpow) then 2 * (fromIntegral maxpow) * (alp !! maxpow)
               else 0, 0
             , 0, 0 )
  in tmFwFinal tm cs params s0 sh0 c0 ch0 .
     tmFwEvalMatrix ai ar alp startp $ mat0
  
tmFwNormalizeInput :: (Ord t, Num t) => t -> t -> t -> (t, t, t, t, Bool)
tmFwNormalizeInput lon0' lat' lon' =
  let _lon = (angNormalize lon0') `angDiff` (angNormalize lon')
      _latsign = signum lat'
      lonsign = signum _lon
      lat = lat' * _latsign
      __lon = _lon * lonsign      
      backside = __lon > 90
      (latsign, lon) =
        if backside
        then
          let l = 180 - __lon
          in if (lat == 0) then (-1, l) else (_latsign, l)
        else (_latsign, __lon)
  in (lat, latsign, lon, lonsign, backside)

tmFwParameters :: GeoFloat f => TraverseMercator f -> f -> f -> (f, f, f, f)
tmFwParameters tm lat lon =
  let e2m = _TMe2m tm
      e2  = _TMe2 tm
      phi = lat ^. toRadians
      lam = lon ^. toRadians
  in if (lat == 90)
     then (0, pi / 2, lam, _TMc tm)
     else
       let c = max 0 (cos lam)
           tau = tan phi
           taup = taupf tm tau
       in (asinh $ (sin lam) / (hypot taup c) -- etap
          , atan2 taup c -- xip
          , atan $ (tanx lam) * taup / (hypot 1 taup)   -- gamma
          , sqrt $ e2m + e2 * (cos phi * cos phi) / (hypot taup c) -- k
          )            


tmFwEvalMatrix :: Fractional t => t
                -> t
                -> [t]
                -> Int
                -> (t, t, t, t, t, t, t, t)
                -> (t, t, (t, t, t, t, t, t, t, t))
tmFwEvalMatrix ar ai _alp n params =
  (ar / 2, ai / 2, tmFwEvalMatrix' ar ai _alp n params)

tmFwEvalMatrix' :: Num t =>
                 t
                 -> t
                 -> [t]
                 -> Int
                 -> (t, t, t, t, t, t, t, t)
                 -> (t, t, t, t, t, t, t, t)                 
tmFwEvalMatrix'
  ar ai alp n (_xi0, _xi1, _eta0, _eta1, _yr0, _yr1, _yi0, _yi1) =
    let xi1  = ar * _xi0 - ai * _eta0 - _xi1 + (alp !! n)
        eta1 = ai * _xi0 + ar * _eta0 - _eta1
        yr1  = ar * _yr0 - ai * _yi0 - _yr1 + 2 * (fromIntegral n) * (alp !! n)
        yi1  = ai * _yr0 + ar * _yi0 - _yi1
            
        n' = pred n
        xi0 = ar * xi1 - ai * eta1 - _xi0 + (alp !! n')
        eta0 = ai * xi1 + ar * eta1 - _eta0
        yr0 = ar * yr1 - ai * yi1 - _yr0 +
              2 * (fromIntegral n') * (alp !! n')
        yi0 = ai * yr1 + ar * yi1 - _yi0
        res = (xi0, xi1, eta0, eta1, yr0, yr1, yi0, yi1)
        n'' = pred n'
    in if (n'' <= 0)
       then res
       else tmFwEvalMatrix' ar ai alp (n'') res

tmFwFinal :: RealFloat t =>
           TraverseMercator t
           -> (t, t, t1, t, Bool)
           -> (t, t, t, t)
           -> t
           -> t
           -> t
           -> t
           -> (t, t, (t, t, t, t, t, t, t, t))
           -> TM t
tmFwFinal tm 
  (_, latsign, _, lonsign, backside) (etap, xip, gamma_, k_) s0 sh0 c0 ch0
  (ar_, ai_, (xi0_, _, eta0_, _, yr0_, yr1_, yi0_, yi1_)) =
  let yr1 = 1 - yr1_ + ar_ * yr0_ - ai_ * yi0_
      yi1 =   - yi1_ + ai_ * yr0_ + ar_ * yi0_
      ar = s0 * ch0
      ai = c0 * sh0
      xi  = xip  + ar * xi0_ - ai * eta0_
      eta = etap + ai * xi0_ + ar * eta0_
      _gamma = ((gamma_ - (atan2 yi1 yr1)) ^. fromRadians) 
      gamma = if (backside) then (180 - _gamma) * latsign * lonsign
              else _gamma * latsign * lonsign
      _k = k_ * (_TMb1 tm) * (hypot yr1 yi1)
      k = _k * (_TMk0 tm)
      y = (_TMa1 tm) * (_TMk0 tm) * latsign * (if backside then pi - xi else xi)
      x = (_TMa1 tm) * (_TMk0 tm) * eta * lonsign
  in TM { _easting = x,
          _northing = y,
          _meridianConvergence = gamma,
          _projectionScale = k
        }



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



