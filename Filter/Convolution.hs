module Filter.Convolution where

import Signals
import Data.Complex
import Fourier

zeroAppend :: [Complex Double] -> [Complex Double]
zeroAppend l = l ++ replicate (length l) 0 


linearConvolution :: [Complex Double] -> [Complex Double] -> [Complex Double]
linearConvolution u h = rfft $ zipWith (*) (fft $ zeroAppend u ) (fft $ zeroAppend h)

--dConv u h = rfft $ zipWith (*) (fft $ zeroAppend u ) (fft $ zeroAppend h)

