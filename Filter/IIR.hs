module Filter.IIR where


import Data.Complex
import Signals
import Fourier
import Filter.Convolution

lowBatterworth :: DSignal -> Double -> DSignal
lowBatterworth (DSignal t0 tau l) b = 
        DSignal t0 tau $ rfft $ zipWith (*) (fft l) $ 
                                            map (\f -> b0 / (1 + 
                                                             a1*exp(-(0 :+ 2*pi*f*tau)) + 
                                                             a2*exp(-(0 :+ 4*pi*f*tau))
                                                            )
                                                ) [0, df .. (n-1)*df]
    where fi = pi / 4
          fact = sin(pi*b*tau)
          q = fact*sin(fi)
          c = 1 - fact*fact
          d = (-c + sqrt(c*c + 4*q*q))/2
          e =  sqrt(d) + sqrt(d+1) --sqrt(d*(d+1))--	
          a1 = -((2*q*q/d - 1)*2/e/e :+ 0)
          a2 = 1/e/e/e/e :+ 0
          b0 = (1 + a1 + a2) 
          n = fromIntegral $ length l
          df = 1 / n / tau
          
          
highBatterworth :: DSignal -> Double -> DSignal
highBatterworth (DSignal t0 tau l) b = 
        DSignal t0 tau $  rfft $ zipWith (*) (fft l) $ 
                                            map (\f -> b0 / (1 + 
                                                             a1*exp(-(0 :+ 2*pi*f*tau)) + 
                                                             a2*exp(-(0 :+ 4*pi*f*tau))
                                                            )
                                                ) [0, df .. (n-1)*df]
    where fi = pi / 4
          fact = sin(pi*b*tau)
          q = fact*sin(fi)
          c = 1 - fact*fact
          d = (-c + sqrt(c*c + 4*q*q))/2
          e = sqrt(d) + sqrt(d+1) --sqrt(d*(d+1))-- 	
          a1 = ((2*q*q/d - 1)*2/e/e :+ 0)
          a2 = 1/e/e/e/e :+ 0
          b0 = (1 + a1 + a2) 
          n = fromIntegral $ length l
          df = 1 / n / tau          
{-
batterworth :: Double -> Double -> [Complex Double]
batterworth b tau = map (\f -> b0 / (1 + 
                                     a1*exp(-(0 :+ 2*pi*f*tau)) + 
                                     a2*exp(-(0 :+ 4*pi*f*tau))
                                    )
                        ) [0,tau ..]
    where fi = pi / 4
          fact = sin(pi*b*tau)
          q = fact*sin(fi)
          c = 1 - fact*fact
          d = (-c + sqrt(c*c + 4*q*q))/2
          e =  sqrt(d) + sqrt(d+1) --sqrt(d*(d+1))--	
          a1 = -((2*q*q/d - 1)*2/e/e :+ 0)
          a2 = 1/e/e/e/e :+ 0
          b0 = (1 + a1 + a2) 
          -}
