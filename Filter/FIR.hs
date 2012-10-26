module Filter.FIR where


import Data.Complex

batterworth :: Double -> Double -> [Complex Double]
batterworth b tau = map (\f -> 1/(1 + (f/b)**2) :+ 0) [0,tau ..]

gaus :: Double -> Double -> [Complex Double]
gaus s tau = map (\f -> exp(-(f*f/2/s/s :+ 0))) [0,tau ..]
