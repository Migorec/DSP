module Fourier where

import Signals
import Data.Complex

dft :: DSignal -> DSignal
dft (DSignal t0 dt ind) =
    DSignal 0 (1 / dt / n) $ map (\k -> sum $ zipWith (\u j -> u * exp (0 :+ (-2*pi*k*j/n))) 
                                                      ind indexes 
                                 ) indexes 
    where n = fromIntegral $ length ind
          indexes = [0 .. n-1]
