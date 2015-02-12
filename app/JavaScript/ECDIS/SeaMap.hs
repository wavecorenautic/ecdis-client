{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module JavaScript.ECDIS.SeaMap (SeaMap, mkSeaMapWidget
                               ,SeaMapZoom, mkSeaMapZoomWidget
                               ,Widget (..)) where

import           Control.Applicative
import           Control.Monad.Trans                  (liftIO)
import           Data.Monoid
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as LT
import           FRP.Sodium
import           FRP.Sodium
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLButtonElement
import           GHCJS.DOM.HTMLCollection
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Node
--import           GHCJS.DOM.Types                      (Node)
import           GHCJS.Foreign
import           JavaScript.CssElementQueries
import           JavaScript.ECDIS.Widget
import           Numeric.Units.Dimensional.TF.NonSI
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Text.Hamlet                          (shamlet)
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

mkSeaMapZoomWidget :: (IsHTMLElement self) => HTMLDocument -> self ->
                      IO (WidgetT SeaMapZoom)
mkSeaMapZoomWidget doc par = newWidget doc par SeaMapZoomWidget

instance Widget SeaMapZoom where
  data WidgetInput SeaMapZoom =
    SeaMapZoomWidget

  newWidget doc par _ = do
    (e, pushE) <- sync $ newEvent


    zin_elem <- fmap castToElement $ htmlElementSetInnerHTML' par zInButton
    zout_elem <- fmap castToElement $ htmlElementSetInnerHTML' par zOutButton

    let ctrlIn = SeaMapZoomInput {
          _initZoom = SeaMapZoom $ 25000 *~ one,
          _zoomFactor = 5000 *~ one
          }
    ctrl <- newControllerIO ctrlIn e

    delZoomIn <- elementOnclick zin_elem $ liftIO . sync $ pushE ZoomIn
    delZoomOut <- elementOnclick zout_elem $ liftIO . sync $ pushE ZoomIn

    let cleanup = do
          delZoomOut
          delZoomIn
          return ()

    return $ Widget (ctrl, cleanup)
    where zInButton, zOutButton :: Text
          zOutButton = "<button>Zoom Out</button>"
          zInButton = "<button>Zoom In</button>"



mkSeaMapWidget :: (IsHTMLElement self) => HTMLDocument -> self -> IO (WidgetT SeaMap)
mkSeaMapWidget doc par = newWidget doc par SeaMapWidget

instance Widget SeaMap where
  data WidgetInput SeaMap = SeaMapWidget

  newWidget doc par _ = do

    parId <- fmap fromJSString $ elementGetId par
    let svgId = T.concat [ parId, "_svgmap" ]
        svgTpl = LT.toStrict $ renderHtml
                 [shamlet|$newline always
                  <svg ##{svgId} .ecdis_map>
                 |]
    svg <- do
      svg_par <- fmap (maybe (error "mkSeaMapWidget: unable to create svg map")
                       castToHTMLElement) $
                 documentCreateElement doc ("div" :: Text)
      htmlElementSetInnerHTML svg_par svgTpl

      fmap (maybe (error "mkSeaMapWidget: unable to append svg map")
            castToHTMLElement) $ nodeAppendChild par (Just svg_par)
    svgmap <- fmap (maybe (error "mkSeaMapWidget: unable to get svg map") id) $
               documentGetElementById doc svgId


    let getSVGDim = do
          w <- elementGetClientWidth  svg
          h <- elementGetClientHeight svg
          return (w *~ one, h *~ one)
    svgDim0 <- getSVGDim
    (svgDim, pushSvgDim) <- sync $ newBehavior svgDim0

    delOnElementResize <- onElementResize svg $ do
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
        viewPortHdlr = elementSetAttribute svgmap ("viewport" :: Text)

    delViewPortListener <- sync . listen (values viewPort) $ viewPortHdlr

    let cleanup = do
          delViewPortListener
          delOnElementResize

    return $ Widget (ctrl, cleanup)

    where
