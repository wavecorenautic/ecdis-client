{-# LANGUAGE TypeFamilies #-}

module JavaScript.ECDIS.Widget where

import           FRP.Sodium
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           Wavecore.ECDIS.Controller

newtype WidgetT w = Widget (ControllerOutput w, IO ())

widgetController :: WidgetT w -> ControllerOutput w
widgetController (Widget (c,_)) = c

widgetCleanup :: WidgetT w -> IO ()
widgetCleanup (Widget (_,cu)) = cu

class (Controller w) => Widget w where
  data WidgetInput w :: *
  data WidgetOutput w :: *

  newWidget :: (IsHTMLElement self) => HTMLDocument -> self -> WidgetInput w ->
               IO (WidgetT w)


