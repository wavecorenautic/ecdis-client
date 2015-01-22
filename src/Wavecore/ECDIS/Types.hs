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


data Clock =
  Clock { _bClockUTCTime :: Behavior TimeOfDay
        , _bClockUTCDate :: Behavior Day
        , _bClockLocalTime :: Behavior TimeOfDay
        , _bClockLocalDate :: Behavior Day
        , _bClockTZ :: Behavior TimeZone
        }

clock :: Behavior UTCTime -> Behavior TimeZone -> Clock
clock bUTCTime bLocalTimeZone =
  let bUTCLocal = fmap (utcToLocalTime utc) bUTCTime
      bTZ = bLocalTimeZone 
      bLT = utcToLocalTime <$> bTZ <*> bUTCTime
  in Clock {
      _bClockUTCTime = fmap localTimeOfDay bUTCLocal,
      _bClockUTCDate = fmap localDay bUTCLocal,
      _bClockLocalTime = fmap localTimeOfDay bLT,
      _bClockLocalDate = fmap localDay bUTCLocal,
      _bClockTZ = bTZ
    }

data SwitchClock =
  SwitchClock { _bSwitchClockTime :: Behavior TimeOfDay
              , _bSwitchClockDate :: Behavior Day
              , _bSwitchClockTZ   :: Behavior TimeZone
              }

switchClock :: Behavior UTCTime -> Behavior TimeZone -> Event () ->
               Reactive SwitchClock
switchClock bT bTZ e =
  let cl = clock bT bTZ
  in do
    bIsUTC <- collectE (\_ s -> (not s, not s)) True e
    let onUTC :: Behavior a -> Behavior a -> Reactive (Behavior (Behavior a))
        onUTC a b = hold a $ fmap (\isUTC -> if isUTC then a else b) bIsUTC
    bDisplayTime <- onUTC (_bClockUTCTime cl) (_bClockLocalTime cl) >>= switch
    bDisplayDate <- onUTC (_bClockUTCDate cl) (_bClockLocalDate cl) >>= switch
    bDisplayTZ <- onUTC (pure utc) (_bClockTZ cl) >>= switch
    return $ SwitchClock {
      _bSwitchClockTime = bDisplayTime,
      _bSwitchClockDate = bDisplayDate,
      _bSwitchClockTZ = bDisplayTZ
      }
