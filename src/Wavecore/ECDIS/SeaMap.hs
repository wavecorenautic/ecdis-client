{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE TypeFamilies    #-}

module Wavecore.ECDIS.SeaMap where

import           Control.Applicative
import           Data.Maybe
import           FRP.Sodium
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()

newtype Coordinate =
  MkCoordinate (PlaneAngle Double, PlaneAngle Double)
  deriving (Eq)

newtype UTMCoordinate =
  MkUTMCoordinate (Length Double, Length Double)
  deriving (Eq)

newtype UTMZone = UTMZone Int
  deriving (Eq)

newtype UTMZonedCoordinate =
  MkZonedCoordinate ( ((Bool, UTMZone),
                       (PlaneAngle Double, Dimensionless Double)),
                     UTMCoordinate)
  deriving (Eq)


class Controller w where
  data ControllerCommand w :: *
  data ControllerInput w :: *
  data ControllerOutput w :: *

  newController :: ControllerInput w -> Event (ControllerCommand w) ->
                   Reactive (ControllerOutput w)

  newControllerIO :: ControllerInput w -> Event (ControllerCommand w) ->
                   IO (ControllerOutput w)
  newControllerIO e i = sync $ newController e i

--
-- SEA MAP
--

data SeaMap

data SeaMapNorthing =
  TrueNorth | MapNorth | HeadingNorth
  deriving (Eq)

newtype SeaMapZoom =
  SeaMapZoom (Dimensionless Int)
  deriving (Eq)

class SeaMapProjection proj where

instance Controller SeaMap where
  data ControllerCommand SeaMap =
    SeaMapToggleInput |
    SeaMapSetNorthing SeaMapNorthing |
    SeaMapSetIntPos Coordinate
    deriving (Eq)
  data ControllerInput SeaMap =
    SeaMapInput {
      _smInPosition :: Behavior (Maybe Coordinate),
      _smInZoomFactor :: Behavior (SeaMapZoom),
      _smInMapSize :: Behavior (Dimensionless Double, Dimensionless Double),
      _smInPixelFactor :: Behavior (Length Double),
      _smInUTMForward :: Coordinate -> UTMZonedCoordinate,
      _smInUTMZonedForward :: UTMZone -> Coordinate -> UTMCoordinate,
      _smInUTMReverse :: Bool -> UTMZone -> UTMCoordinate -> Coordinate,
      _smInExtHeading :: Behavior (Maybe (PlaneAngle Double))
      }

  data ControllerOutput SeaMap =
    SeaMapOutput {
      _smUTMForward :: Behavior (Coordinate -> UTMCoordinate),
      _smUTMReverse :: Behavior (UTMCoordinate -> Coordinate),
      _smExtPosAvail :: Behavior Bool,
      _smExtHeadingAvail :: Behavior Bool,
      _smCurrentPosition :: Behavior (Coordinate),
      _smCurrentPositionIsExternal :: Behavior Bool,
      _smZone :: Behavior (UTMZone),
      _smIsNorth :: Behavior Bool,
      _smMeridianConvergence :: Behavior (PlaneAngle Double),
      _smProjectionScale :: Behavior (Dimensionless Double),
      _smRotation :: Behavior (PlaneAngle Double),
      _smNorthing :: Behavior SeaMapNorthing,
      _smMapOrigin :: Behavior (Length Double, Length Double),
      _smMapDim    :: Behavior (Length Double, Length Double)
      }


  newController i e =
    let (toggleInputE, setNorthingE, setIntPosE) = seaMapSplitInputEvents e
        extPosAvailE = fmap isJust $ updates (_smInPosition i)
        extHeadingAvailE = fmap isJust $ updates (_smInExtHeading i)

    in do
      -- external inputs
      extPosAvail <- hold False extPosAvailE
      extHeadingAvail <- hold False extHeadingAvailE

      -- the switching of position input
      intPos <- hold smDefaultCoord $ fmap (\(SeaMapSetIntPos p) -> p) setIntPosE
      (isExternal, pushIsExternal) <- newBehavior False
      let checkInputs :: () -> Reactive (Behavior Coordinate)
          checkInputs _ = do
            extAvail <- sample extPosAvail
            if (not extAvail)
              then (pushIsExternal False >> return intPos)
              else do
                isExt <- sample isExternal
                if (isExt) then (pushIsExternal False >> return intPos)
                else do
                  pushIsExternal True
                  let frJust =
                        maybe (error "SeaMap Controller: no external pos") id
                  return $ fmap frJust $ _smInPosition i
          switchPosSrcE :: Event (Behavior Coordinate)
          switchPosSrcE = execute $ fmap checkInputs $
            let toggle = fmap (\_ -> ()) toggleInputE
                extLost = fmap (\_ -> ()) $ filterE (not) (updates extPosAvail)
            in toggle `merge` extLost
      posSrcSwitch <- hold intPos $ switchPosSrcE
      curPos <- switch posSrcSwitch

      -- position and map transformation
      let curPosUTMZoned' = fmap (_smInUTMForward i) curPos
          curPosUTMZoned  = fmap (\(MkZonedCoordinate a) -> a) curPosUTMZoned'
          northp = fmap (fst.fst.fst) curPosUTMZoned
          zone = fmap (snd.fst.fst) curPosUTMZoned
          meridianConvergence = fmap (fst.snd.fst) curPosUTMZoned
          projectionScale = fmap (snd.snd.fst) curPosUTMZoned
          curPosUTM = fmap snd curPosUTMZoned
          utmRv = (_smInUTMReverse i) <$> northp <*> zone
          utmFw = (_smInUTMZonedForward i) <$> zone

      -- map viewport
      let pxZoom =
            liftA2 (\px (SeaMapZoom z) -> px * fmap fromIntegral z)
              (_smInPixelFactor i) (_smInZoomFactor i)
          mapWidth = liftA2 (*) pxZoom . fmap fst $
                      (_smInMapSize i)
          mapWidth2 = liftA2 (/) mapWidth (pure _2)
          mapHeight = liftA2 (*) pxZoom . fmap snd $
                      (_smInMapSize i)
          mapHeight2 = liftA2 (/) mapHeight (pure _2)
          mapDim = liftA2 (\a b -> (a,b)) mapWidth mapHeight
          mapOrigin = liftA3 (\(MkUTMCoordinate (x,y)) w2 h2 -> (x - w2, y - h2))
                        curPosUTM mapWidth2 mapHeight2

      -- rotation / heading
      let mapNorth = pure $ 0 *~ degree
          trueNorth = liftA2 (-) mapNorth meridianConvergence
          headingLostE = fmap (\_ -> SeaMapSetNorthing MapNorth) $
                          filterE not . updates $ extHeadingAvail
          northingChange = headingLostE `merge` setNorthingE
      (northingSwitch, pushNorthingSwitch) <- newBehavior MapNorth
      let frJust = maybe (error "SeaMap Controller: no extern heading") id
          onNorthingChange (SeaMapSetNorthing n) =
            case n of
              TrueNorth -> do pushNorthingSwitch TrueNorth >> return trueNorth
              MapNorth -> do pushNorthingSwitch MapNorth >> return mapNorth
              HeadingNorth -> do
                extAvail <- sample extHeadingAvail
                if (extAvail)
                then do pushNorthingSwitch HeadingNorth
                        return $ fmap frJust (_smInExtHeading i)
                else do pushNorthingSwitch MapNorth >> return mapNorth
          onNorthingChange _ =
            error "SeaMap Controller: unexpxted Northing event"
          northingSwitchToE = execute $ fmap onNorthingChange northingChange
      rotationSwitch <- hold mapNorth northingSwitchToE
      rotation <- switch rotationSwitch

      return $ SeaMapOutput {
        _smExtPosAvail = extPosAvail,
        _smExtHeadingAvail = extHeadingAvail,
        _smCurrentPosition = curPos,
        _smCurrentPositionIsExternal = isExternal,
        _smZone = zone,
        _smIsNorth = northp,
        _smMeridianConvergence = meridianConvergence,
        _smProjectionScale = projectionScale,
        _smRotation = rotation,
        _smNorthing = northingSwitch,
        _smUTMReverse = utmRv,
        _smUTMForward = utmFw,
        _smMapOrigin = mapOrigin,
        _smMapDim = mapDim
        }

smDefaultCoord :: Coordinate
smDefaultCoord = MkCoordinate (0 *~ degree, 0 *~ degree)

seaMapSplitInputEvents :: Event (ControllerCommand SeaMap) ->
                          ( Event (ControllerCommand SeaMap)
                          , Event (ControllerCommand SeaMap)
                          , Event (ControllerCommand SeaMap)
                          )
seaMapSplitInputEvents e =
  let toggleInputE = filterE ((==) SeaMapToggleInput) e
      setNorthingE = filterE (\e' -> case e' of
                                      SeaMapSetNorthing _ -> True
                                      _ -> False) e
      setIntPosE   = filterE (\e' -> case e' of
                                      SeaMapSetIntPos _ -> True
                                      _ -> False) e
  in (toggleInputE, setNorthingE, setIntPosE)



data SeaMapControls

instance Controller SeaMapControls where

