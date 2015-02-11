{-# LANGUAGE TypeFamilies #-}

module JavaScript.ECDIS.Widget where

import           FRP.Sodium
import           GHCJS.DOM.HTMLElement
import           Wavecore.ECDIS.Controller

newtype WidgetT w = Widget (ControllerOutput w, IO ())

class (Controller w) => Widget w where
  data WidgetInput w :: *
  data WidgetOutput w :: *

  newWidget :: (IsHTMLElement self) => self -> WidgetInput w -> IO (WidgetT w)


