module Filter.FIR where


import Data.Complex
import Signals
import Fourier


lowBatterworth :: Double -> DSignal -> DSignal
lowBatterworth b (DSignal t0 tau l) = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f -> 1/(1+(f/b)**2) :+ 0) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau


highBatterworth :: Double -> DSignal -> DSignal
highBatterworth b (DSignal t0 tau l) = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f -> 1/(1+(b/f)**2) :+ 0) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau

lowGaus :: Double -> DSignal -> DSignal
lowGaus b (DSignal t0 tau l) = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f -> exp ((-f*f/2/b/b):+0)) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau

highGaus :: Double -> DSignal -> DSignal
highGaus b (DSignal t0 tau l) = DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map (\f ->1 - exp ((-f*f/2/b/b) :+ 0)) [0,df..(n-1)*df]
    where n = fromIntegral $ length l
          df = 1 / n / tau

