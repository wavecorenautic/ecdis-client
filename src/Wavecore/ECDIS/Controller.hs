{-# LANGUAGE TypeFamilies #-}

module Wavecore.ECDIS.Controller where

import           FRP.Sodium

class Controller w where
  data ControllerCommand w :: *
  data ControllerInput w :: *
  data ControllerOutput w :: *

  newController :: ControllerInput w -> Event (ControllerCommand w) ->
                   Reactive (ControllerOutput w)

  newControllerIO :: ControllerInput w -> Event (ControllerCommand w) ->
                   IO (ControllerOutput w)
  newControllerIO e i = sync $ newController e i

