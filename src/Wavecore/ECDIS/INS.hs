module Wavecore.ECDIS.INS where

import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Time
import FRP.Sodium
import Wavecore.ECDIS.Types




data INSInput =
  INSInput { _bInsTime :: Behavior UTCTime
           , _eInsPos :: Event (SensorId, Coordinate)
           , _eInsLog :: Event (SensorId, SMG)
           , _eInsCourse :: Event (SensorId, COG)             
           }

data INS =
  INS { _bInsPos :: Behavior Coordinate -- | position
      , _bInsSOG :: Behavior SOG -- | speed over ground
      , _bInsSMG :: Behavior SMG -- | speed made good
      , _bInsCOG :: Behavior COG -- | course over ground
      , _bInsCMG :: Behavior CMG -- | course made good
      }


x :: Coordinate -> Coordinate -> Length Double
x a b = undefined

ins :: INSInput -> Reactive INS
ins i = do

  return $ INS {
    _bInsPos = undefined,
    _bInsSOG = undefined,
    _bInsSMG = undefined,
    _bInsCOG = undefined,
    _bInsCMG = undefined
    }


type INSState = (UTCTime, (Coordinate, SMG, COG))


