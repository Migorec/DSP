module Filter.IIR (highButterworth, lowButterworth) where


import Data.Complex
import Signals
import Fourier
import Filter.Convolution

sFilter :: DSignal -> (Double -> Complex Double) -> DSignal
sFilter (DSignal t0 tau l) h = 
    DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ map h [0,df .. (n-1)*df]
        where n = fromIntegral $ length l
              df = 1 / n / tau

iirResponse b0 a1 a2 tau f =  b0 / (1 + a1*exp(-(0 :+ 2*pi*f*tau)) + a2*exp(-(0 :+ 4*pi*f*tau)))

butterworthCoef tau b = (b0, a1,a2) 
    where fi = pi / 4
          fact = sin(pi*b*tau)
          q = fact*sin(fi)
          c = 1 - fact*fact
          d = (-c + sqrt(c*c + 4*q*q))/2
          e =  sqrt(d) + sqrt(d+1) 
          a1 = (2*q*q/d - 1)*2/e/e :+ 0
          a2 = 1/e/e/e/e :+ 0
          b0 = 1 + a1 + a2

lowButterworth :: Double -> DSignal -> DSignal
lowButterworth b sig@(DSignal t0 tau l) = sFilter sig (iirResponse (b0-2*a1) (-a1) a2 tau)
    where 
          (b0,a1,a2) = butterworthCoef tau b
          
highButterworth :: Double -> DSignal -> DSignal
highButterworth b sig@(DSignal t0 tau l) = sFilter sig (iirResponse b0 a1 a2 tau)
    where 
          (b0,a1,a2) = butterworthCoef tau b          

