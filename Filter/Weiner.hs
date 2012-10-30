module Filter.Weiner where

import Data.Complex
import Signals
import Fourier

weinerResponse :: [Complex  Double] -> [Complex Double] -> [Complex Double]
weinerResponse betas kapas = zipWith (\b k -> (b*b - k*k)/b/b) betas kapas

weiner ::[Complex Double] -> DSignal -> DSignal
weiner err (DSignal t0 dt l)  = DSignal t0 dt $ rfft $ zipWith (*) ffl $ weinerResponse ffl (fft err)
    where ffl = fft l
