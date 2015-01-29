
module Wavecore.ECDIS.Types where

import Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Geodetic
import Data.Text (Text)

type Coordinate = GeodeticCoordinate WGS84 Double

mkCoord :: PlaneAngle Double -> PlaneAngle Double -> Length Double ->
                Coordinate
mkCoord = WGS84


-- | Speed Over Ground
newtype SOG = SOG (Velocity Double) deriving (Eq, Ord, Show)

-- | Speed Made Good
newtype SMG = SMG  (Velocity Double) deriving (Eq, Ord, Show)

-- | Velocity Made Good
newtype VMG = VMG (Velocity Double) deriving (Eq, Ord, Show)


-- | Course Over Ground
newtype COG = COG (PlaneAngle Double) deriving (Eq, Ord, Show)

-- | Course Made Good
newtype CMG = CMG (PlaneAngle Double) deriving (Eq, Ord, Show)



type SensorId = Text

newtype Waypoint = Waypoint Coordinate

data Route = Route {
  _routeName :: Text,
  _routeWaypoints :: [Waypoint]
  }

  
