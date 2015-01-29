module Wavecore.ECDIS.ECDIS where

import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Time
import FRP.Sodium
import Control.Applicative
import Wavecore.ECDIS.Types
import Wavecore.ECDIS.Clock
import Wavecore.ECDIS.INS


data ECDISCmd =
  SwitchClockMode -- | switch clock between 'LocalTime' and 'UTCTime'
  deriving (Eq, Show)

data ECIDS =
  ECIDS { _bECIDSClock :: SwitchClock
        , _bECIDSIns :: INS
        }

resetPos :: Coordinate
resetPos = mkCoord (0 *~ degree) (0 *~ degree) (0 *~ meter)

class ECIDSSystem sys where
  mkTimeB :: sys -> Reactive (Behavior UTCTime)
  mkPosB  :: sys -> Reactive (Behavior Coordinate)
  mkCmdE  :: sys -> Reactive (Event ECDISCmd)
  mkPosEs :: sys -> Reactive (Event (SensorId, Coordinate))
  mkLogEs :: sys -> Reactive (Event (SensorId, SMG))
  mkCourseEs :: sys -> Reactive (Event (SensorId, COG))
  mkINS :: sys -> Behavior UTCTime -> Reactive INS
  mkINS sys bt = do
    posEs <- mkPosEs sys
    logEs <- mkLogEs sys
    courseEs <- mkCourseEs sys
    ins $ INSInput {
      _bInsTime = bt,
      _eInsPos = posEs,
      _eInsLog = logEs,
      _eInsCourse = courseEs
      }    
  mkEcids :: sys -> Reactive ECIDS
  mkEcids sys = do
    -- the time
    bt <- mkTimeB sys

    -- the cmd events
    e <- mkCmdE sys

    -- the INS
    myIns <- mkINS sys bt
    
    -- set internal position to 0/0 until INS delivers valid data
    let bPosInt = _bInsPos myIns

    -- the switched clock
    let eSwClk = cmdEvent SwitchClockMode e        
    cl <- clock bt (fmap lookupTimeZone bPosInt) >>= switchClock eSwClk
    
    return $ ECIDS { _bECIDSClock = cl
                   , _bECIDSIns = myIns
                   }
  

cmdEvent :: (Eq cmd) => cmd -> Event cmd -> Event ()
cmdEvent p =
  let f e = if (e == p) then Just () else Nothing
  in filterJust . fmap f



-- TODO: implement this
-- find more: https://stackoverflow.com/questions/16086962/how-to-get-a-time-zone-from-a-location-using-latitude-and-longitude-coordinates    
lookupTimeZone :: Coordinate -> TimeZone
lookupTimeZone c = utc




