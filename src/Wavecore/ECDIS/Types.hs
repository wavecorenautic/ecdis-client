{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RecursiveDo #-}

module Wavecore.ECDIS.Types where

import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Control.Applicative
import FRP.Sodium
import Data.Time
import Data.Geodetic
import Data.Text (Text)

type Coordinate = GeodeticCoordinate WGS84 Double
type SensorId = Text


data ECIDS =
  ECIDS { _bECIDSClock :: SwitchClock
        }

ecids :: Behavior UTCTime -> Behavior Coordinate -> Event () -> Reactive ECIDS
ecids bt bp e = do
  cl <- clock bt (fmap lookupTimeZone bp) >>= switchClock e
  return $ ECIDS { _bECIDSClock = cl }
  

lookupTimeZone :: Coordinate -> TimeZone
lookupTimeZone c = utc -- TODO lookup timezone on coordinate

data Clock =
  Clock { _bClockUTCTime :: Behavior TimeOfDay
        , _bClockUTCDate :: Behavior Day
        , _bClockLocalTime :: Behavior TimeOfDay
        , _bClockLocalDate :: Behavior Day
        , _bClockTZ :: Behavior TimeZone
        }

clock :: Behavior UTCTime -> Behavior TimeZone -> Reactive Clock
clock bUTCTime bLocalTimeZone =
  let bUTCLocal = fmap (utcToLocalTime utc) bUTCTime
      bTZ = bLocalTimeZone 
      bLT = utcToLocalTime <$> bTZ <*> bUTCTime
  in do
    _but <- mkOnChange $ fmap localTimeOfDay bUTCLocal
    _bud <- mkOnChange $ fmap localDay bUTCLocal
    _blt <- mkOnChange $ fmap localTimeOfDay bLT
    _bld <- mkOnChange $ fmap localDay bLT
    return $ Clock {
      _bClockUTCTime = _but,
      _bClockUTCDate = _bud,
      _bClockLocalTime = _blt,
      _bClockLocalDate = _bld,
      _bClockTZ = bTZ
    }
  
data SwitchClock =
  SwitchClock { _bSwitchClockTime  :: Behavior TimeOfDay
              , _bSwitchClockDate  :: Behavior Day
              , _bSwitchClockTZ    :: Behavior TimeZone
              , _bSwitchClockIsUTC :: Behavior Bool
              , _bSwitchClockInnerClock :: Clock
              }

switchClock :: Event () -> Clock -> Reactive SwitchClock
switchClock e cl = do
  bIsUTC <- collectE (\_ s -> (not s, not s)) True e
  let onUTC :: Behavior a -> Behavior a -> Reactive (Behavior (Behavior a))
      onUTC a b = hold a $ fmap (\isUTC -> if isUTC then a else b) bIsUTC
  bDisplayTime <- onUTC (_bClockUTCTime cl) (_bClockLocalTime cl)
                  >>= switch >>= mkOnChange
  bDisplayDate <- onUTC (_bClockUTCDate cl) (_bClockLocalDate cl)
                  >>= switch >>= mkOnChange
  bDisplayTZ <- onUTC (pure utc) (_bClockTZ cl)
                >>= switch >>= mkOnChange                                    
  return $ SwitchClock {
    _bSwitchClockTime = bDisplayTime,
    _bSwitchClockDate = bDisplayDate,
    _bSwitchClockTZ = bDisplayTZ,
    _bSwitchClockIsUTC = fmap ((==) utc) bDisplayTZ,
    _bSwitchClockInnerClock = cl
    }



mkOnChange :: Eq a => Behavior a -> Reactive (Behavior a)
mkOnChange b = do
  b0 <- sample b
  hold b0 $ snapshot (\b1 _ -> b1) (onChange b) b
  
  
onChange :: Eq a => Behavior a -> Event a
onChange b =
  filterJust $
  snapshot (\old new -> if (old == new)
                        then Nothing
                        else Just new) (updates b) b
                                                            
