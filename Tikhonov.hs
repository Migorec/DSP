module Tikhonov where


import Data.Complex
import Data.List (zipWith4)
import Numeric.Tools.Equation
import Fourier

delta = 0.05
epsilon = 0.05

gamma :: Double -> [Complex Double] -> [Complex Double] -> Double -> Double
gamma dx u1 u2 alpha = 
    dx / n * (sum $ zipWith3 (\u1 u2 m -> (magnitude u2) ** 2 * 
                                          dx*dx * 
                                          (magnitude u1) ** 2 *
                                          (1+2*pi*m/t) ** 2 /
                                          ((magnitude u2) ** 2 * dx * dx +   
                                           alpha * (1 + (2*pi*m/t)**2))**2  
                             ) u1 u2 [0 .. n-1])
    where n = fromIntegral $ length u1 
          t = dx*(n-1)
          
          
beta :: Double -> [Complex Double] -> [Complex Double] -> Double -> Double
beta dx u1 u2 alpha = 
     dx / n * (sum $ zipWith3 (\u1 u2 m -> alpha*alpha*
                                           (1 + (2*pi*m/t)**2) *
                                           (magnitude u1) ** 2 /
                                           ((magnitude u2) ** 2 * dx * dx +   
                                            alpha * (1 + (2*pi*m/t)**2))**2
                             ) u1 u2 [0 .. n-1])
    where n = fromIntegral $ length u1 
          t = dx*(n-1)
          
          
rho :: Double -> [Complex Double] -> [Complex Double] -> Double -> Double
rho dx u1 u2 alpha = beta dx u1 u2 alpha - (delta + epsilon * (sqrt $ gamma dx u1 u2 alpha)) ** 2

getAlpha :: Double -> [Complex Double] -> [Complex Double] -> Double
getAlpha dx u1 u2 = case r of
                     NotBracketed -> error "not bracketed!"
                     SearchFailed -> error "search failed!"
                     Root x -> x
    where r = solveBisection 0.001 (0.001, 10) $  rho dx u1 u2  
    
    
tikhonov :: Double -> [Complex Double] -> [Complex Double] -> [Complex Double]
tikhonov dx u1 u2 = (drop (round (n/2)) hs)++ (take (round (n/2)) hs)
    
  where alpha = getAlpha dx fu1 fu2
        --fu1 = fft $ zipWith (*) u1 $ map (\k -> exp (0 :+ pi*k)) [0..n-1]
        --fu2 = fft $ zipWith (*) u2 $ map (\k -> exp (0 :+ pi*k)) [0..n-1]
        fu1 = fft u1
        fu2 = fft u2
        n = fromIntegral $ length u1 
        t = dx*(n-1)
        hs = map (\k -> (dx/n :+ 0) * 
               (sum $ zipWith4 (\fu1 fu2 u2 m -> (conjugate fu2 * fu1 /
                                             (((magnitude fu2)**2 * dx*dx + 
                                               alpha*(1+ (2*pi*m/t)**2)
                                              ) :+ 0)                                                         
                                              * exp (0 :+ (2*pi*k*m/n))
                                            )
                               ) fu1 fu2 u2 [0..n-1])
                )[0..n-1]
    
