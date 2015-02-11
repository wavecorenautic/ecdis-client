{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module JavaScript.ECDIS.SeaMap (SeaMap, mkSeaMapWidget, Widget (..)) where

import           Control.Applicative
import           Data.Monoid
import           Data.Text                            (Text)
import           FRP.Sodium
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLCollection
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Types                      (Node)
import           GHCJS.Foreign
import           JavaScript.CssElementQueries
import           JavaScript.ECDIS.Widget
import           Numeric.Units.Dimensional.TF.NonSI
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P
import           Wavecore.ECDIS.Controller
import           Wavecore.ECDIS.SeaMap


htmlElementSetInnerHTML' :: (IsHTMLElement p, ToJSString c) =>
                           p -> c -> IO Node
htmlElementSetInnerHTML' par content = do
    htmlElementSetInnerHTML par content
    cs <- fmap (maybe (error "htmlElementSetInnerHTML': unable to get children")
                id) $ htmlElementGetChildren par
    fmap (maybe (error "htmlElementSetInnerHTML': unable to get child 0") id) $
      htmlCollectionItem cs 0



mkSeaMapWidget :: (IsHTMLElement self) => self -> IO (WidgetT SeaMap)
mkSeaMapWidget par = newWidget par SeaMapWidget

instance Widget SeaMap where
  data WidgetInput SeaMap = SeaMapWidget

  newWidget par i = do
    svg <- fmap castToElement $ htmlElementSetInnerHTML' par svgTemplate

    let getSVGDim = do
          w <- elementGetClientWidth  svg
          h <- elementGetClientHeight svg
          return (w *~ one, h *~ one)
    svgDim0 <- getSVGDim
    (svgDim, pushSvgDim) <- sync $ newBehavior svgDim0

    delOnElementResize <- onElementResize par $ do
      svgDim' <- getSVGDim
      sync $ pushSvgDim svgDim'

    (e, pushE) <- sync $ newEvent

    let ctrl_in = SeaMapInput {
          _smInPosition = pure Nothing,
          _smInZoomFactor = pure . SeaMapZoom $ 25000 *~ one,
          _smInMapSize = svgDim,
          _smInPixelFactor = pure $ ((P./) 1 96) *~ inch,
          _smInExtHeading = pure Nothing
          }

    ctrl <- newControllerIO ctrl_in e

    let viewPort = liftA2 (\(x,y) (w,h) ->
                            mconcat [ show ((round $ x /~ meter) :: Int), " "
                                    , show ((round $ y /~ meter) :: Int), " "
                                    , show ((round $ w /~ meter) :: Int), " "
                                    , show ((round $ h /~ meter) :: Int) ])
                   (_smMapOrigin ctrl) (_smMapDim ctrl)
        viewPortHdlr = elementSetAttribute svg ("viewport" :: Text)

    delViewPortListener <- sync . listen (values viewPort) $ viewPortHdlr

    let cleanup = do
          delViewPortListener
          delOnElementResize

    return $ Widget (ctrl, cleanup)

    where svgStyle, svgTemplate :: Text
          svgStyle = "min-width:250px;min-height:250px;height:100%;width:100%"
          svgTemplate = mconcat [ "<svg style='", svgStyle, "' />" ]
