module Wavecore.ECDIS.Internal where

import FRP.Sodium


mkOnChange :: Eq a => Behavior a -> Reactive (Behavior a)
mkOnChange b = do
  b0 <- sample b
  hold b0 $ snapshot (\b1 _ -> b1) (onChange b) b
  
  
onChange :: Eq a => Behavior a -> Event a
onChange b =
  filterJust $
  snapshot (\old new -> if (old == new)
                        then Nothing
                        else Just new) (updates b) b
                                                            

