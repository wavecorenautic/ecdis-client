module Wavecore.ECDIS.INS where

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
  INS { _bInsSOG :: Behavior (Maybe SOG) -- | speed over ground
      , _bInsSMG :: Behavior (Maybe SMG) -- | speed made good
      , _bInsCOG :: Behavior (Maybe COG) -- | course over ground
      , _bInsCMG :: Behavior (Maybe CMG) -- | course made good
      }

ins :: INSInput -> Reactive INS
ins i = undefined
