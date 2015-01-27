module Wavecore.ECDIS.ECDIS where

import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Time
import FRP.Sodium
import Wavecore.ECDIS.Types
import Wavecore.ECDIS.Clock
import Wavecore.ECDIS.INS



data ECDISCmd =
  SwitchClockMode -- | switch clock between 'LocalTime' and 'UTCTime'
  deriving (Eq, Show)

data ECIDS =
  ECIDS { _bECIDSClock :: SwitchClock
        }

class ECIDSSystem sys where
  mkTimeB :: sys -> Reactive (Behavior UTCTime)
  mkPosB  :: sys -> Reactive (Behavior Coordinate)
  mkCmdE  :: sys -> Reactive (Event ECDISCmd)
  mkPosEs :: sys -> Reactive (Event (SensorId, Coordinate))
  mkLogEs :: sys -> Reactive (Event (SensorId, SMG))
  mkCourseEs :: sys -> Reactive (Event (SensorId, COG))
  mkEcids :: sys -> Reactive ECIDS
  mkEcids sys = do
    bt <- mkTimeB sys
    e <- mkCmdE sys
    posEs <- mkPosEs sys
    logEs <- mkLogEs sys
    courseEs <- mkCourseEs sys

    myIns <- ins $ INSInput {
      _bInsTime = bt,
      _eInsPos = posEs,
      _eInsLog = logEs,
      _eInsCourse = courseEs
      }
             
    let bpos = undefined
    let eSwClk = cmdEvent SwitchClockMode e        
    cl <- clock bt (fmap lookupTimeZone bpos) >>= switchClock eSwClk
    return $ ECIDS { _bECIDSClock = cl }
  

cmdEvent :: (Eq cmd) => cmd -> Event cmd -> Event ()
cmdEvent p =
  let f e = if (e == p) then Just () else Nothing
  in filterJust . fmap f



-- TODO: implement this
-- find more: https://stackoverflow.com/questions/16086962/how-to-get-a-time-zone-from-a-location-using-latitude-and-longitude-coordinates    
lookupTimeZone :: Coordinate -> TimeZone
lookupTimeZone c = utc




