{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE TypeFamilies    #-}

module Wavecore.ECDIS.SeaMap where

import           Control.Applicative
import           Data.Geo.TransverseMercator
import           Data.Geo.UTM
import           Data.Maybe
import           FRP.Sodium
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P ()
import           Wavecore.ECDIS.Controller

newtype Coordinate =
  MkCoordinate (PlaneAngle Double, PlaneAngle Double)
  deriving (Eq)

newtype UTMCoordinate =
  MkUTMCoordinate (Length Double, Length Double)
  deriving (Eq)

newtype UTMZone = UTMZone Int
  deriving (Eq)

newtype UTMZonedCoordinate =
  MkZonedCoordinate ( ((UTMZone, Bool),
                       (PlaneAngle Double, Dimensionless Double)),
                     UTMCoordinate)
  deriving (Eq)


_utmForward :: Coordinate -> UTMZonedCoordinate
_utmForward (MkCoordinate (lat, lon)) =
  let ((z',n), (TM x y conv scale)) =
        maybe (error "utmForward: no result") id $
        utmForward' lat lon
      utm = MkUTMCoordinate (x, y)
      cs = (conv, scale)
      zn = (UTMZone z', n)
  in MkZonedCoordinate ((zn, cs), utm)

_utmZonedForward :: UTMZone -> Coordinate -> UTMCoordinate
_utmZonedForward (UTMZone z) (MkCoordinate (lat,lon)) =
  let (_, (TM x y _ _)) =
        maybe (error "utmZonedForward: no result") id $
        utmZonedForward z lat lon
  in MkUTMCoordinate (x, y)


_utmReverse :: Bool -> UTMZone -> UTMCoordinate -> Coordinate
_utmReverse n (UTMZone z) (MkUTMCoordinate (x,y)) =
  MkCoordinate $  maybe (error "utmReverse: no result") id $
  utmReverse z n x y




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
      let curPosUTMZoned' = fmap _utmForward curPos
          curPosUTMZoned  = fmap (\(MkZonedCoordinate a) -> a) curPosUTMZoned'
          northp = fmap (snd.fst.fst) curPosUTMZoned
          zone = fmap (fst.fst.fst) curPosUTMZoned
          meridianConvergence = fmap (fst.snd.fst) curPosUTMZoned
          projectionScale = fmap (snd.snd.fst) curPosUTMZoned
          curPosUTM = fmap snd curPosUTMZoned
          utmRv = _utmReverse <$> northp <*> zone
          utmFw = _utmZonedForward <$> zone

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
smDefaultCoord = MkCoordinate (52.3 *~ degree, 7.1 *~ degree)

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




instance Controller SeaMapZoom where
  data ControllerCommand SeaMapZoom = ZoomIn | ZoomOut
  data ControllerInput SeaMapZoom =
    SeaMapZoomInput {
      _initZoom :: SeaMapZoom,
      _zoomFactor :: (Dimensionless Int)
      }
  data ControllerOutput SeaMapZoom =
    SeaMapZoomOutput {
      _seaMapZoom :: Behavior SeaMapZoom
      }

  newController i e =
    let zoomF ZoomIn (SeaMapZoom z)
          = SeaMapZoom $ z - (_zoomFactor i)
        zoomF ZoomOut (SeaMapZoom z)
          = SeaMapZoom $ z + (_zoomFactor i)
    in do
      a <- accum (_initZoom i) (fmap zoomF e)
      return SeaMapZoomOutput {
        _seaMapZoom = a
        }





