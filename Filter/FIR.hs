module Filter.FIR where


import Data.Complex
import Signals
import Fourier


lowBatterworth :: DSignal -> Double -> DSignal
lowBatterworth (DSignal t0 tau l) b = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f -> 1/(1+(f/b)**2) :+ 0) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau


highBatterworth :: DSignal -> Double -> DSignal
highBatterworth (DSignal t0 tau l) b = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f -> 1/(1+(b/f)**2) :+ 0) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau

lowGaus :: DSignal -> Double -> DSignal
lowGaus (DSignal t0 tau l) b = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f -> exp ((-f*f/2/b/b):+0)) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau

highGaus :: DSignal -> Double -> DSignal
highGaus (DSignal t0 tau l) b = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f ->1 - exp ((-f*f/2/b/b) :+ 0)) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau

