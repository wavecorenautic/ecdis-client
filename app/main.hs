{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where
import           Control.Applicative
import           Data.Monoid
import           Data.Text                            (Text)
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLCollection
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Types                      (Node)
import           GHCJS.Foreign
import           JavaScript.CssElementQueries
import           Numeric.Units.Dimensional.TF.NonSI
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P
import           Wavecore.ECDIS.SeaMap


class (Controller w) => Widget w where
  data WidgetInput w :: *
  data WidgetOutput w :: *

  newWidget' :: (IsHTMLElement self) => self -> WidgetInput w ->
                IO (ControllerInput w, Event (ControllerCommand w))
  newWidget :: (IsHTMLElement self)  => self -> WidgetInput w ->
               IO (ControllerOutput w)
  newWidget par i = do
    (ctrl_in, e) <- newWidget' par i
    newControllerIO ctrl_in e

htmlElementSetInnerHTML' :: (IsHTMLElement p, ToJSString c) =>
                           p -> c -> IO Node
htmlElementSetInnerHTML' par content = do
    htmlElementSetInnerHTML par content
    cs <- fmap (maybe (error "htmlElementSetInnerHTML': unable to get children")
                id) $ htmlElementGetChildren par
    fmap (maybe (error "htmlElementSetInnerHTML': unable to get child 0") id) $
      htmlCollectionItem cs 0

instance Widget SeaMap where
  data WidgetInput SeaMap =
    SeaMapWidget

  newWidget' par i = do
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
          _smInUTMForward = undefined,
          _smInUTMZonedForward = undefined,
          _smInUTMReverse = undefined,
          _smInExtHeading = pure Nothing
          }

    return (ctrl_in, e)

    where svgStyle, svgTemplate :: Text
          svgStyle = "min-width:250px;min-height:250px;height:100%;width:100%"
          svgTemplate = mconcat [ "<svg style='", svgStyle, "' />" ]



main :: IO ()
main = runWebGUI $ \ webView -> do
  enableInspector webView
  doc <- fmap (maybe (error "ecdis: unable to get dom document") id) $
         webViewGetDomDocument webView
  body <- fmap (maybe (error "ecdis: unable to get dom body") id) $
          documentGetBody doc

  par <- fmap
         (maybe (error "ecdis: unable to find ecdis_seamap element")
          castToHTMLElement)
         $ documentGetElementById doc ("ecdis_seamap" :: Text)

  seaMap <- newWidget par SeaMapWidget

  return ()



  -- cleanup handler
