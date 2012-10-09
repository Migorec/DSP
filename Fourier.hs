module Fourier where

import Signals
import Service
import Data.Complex
import Data.Ratio
import Data.Map (Map, fromList, (!))

dft :: [Complex Double] -> [Complex Double]
dft l =map (\k -> sum $ zipWith (\u j -> u * exp (0 :+ (-2*pi*k*j/n))) 
                                                      l indexes 
                                 ) indexes 
    where n = fromIntegral $ length l
          indexes = [0 .. n-1]
          
          
tFactor :: (Integral a, Integral b) => a -> b -> Complex Double
tFactor nk n = exp (0 :+ (-2*pi* fromIntegral nk / fromIntegral n))
          
          
fft' :: [Complex Double] -> Int -> Map (Ratio Int) (Complex Double) -> [Complex Double]
fft' [s0,s1] _ ws = [s0 + (ws ! (0 % 2)) * s1, s0 - (ws ! (0 % 2)) * s1]
fft' l n ws | odd n = dft l 
            | otherwise = p1 ++ p2 
    where n1 = n `div` 2
          (u0,u1) = oddEven l
          s0 = fft' u0 n1 ws
          s1 = fft' u1 n1 ws
          si = [0 .. n1 - 1]
          p1 = zipWith3 (\s0 s1 k -> s0 + s1*(ws ! (k % n))) s0 s1 si 
          p2 = zipWith3 (\s0 s1 k -> s0 - s1*(ws ! (k % n))) s0 s1 si 
          
fft :: [Complex Double] -> [Complex Double]
fft l | odd n = dft l
      | otherwise = fft' l n ws
    where n = length l
          wCount = n `div` 2 
          ws = fromList $ map (\i -> (i % n, tFactor i n)) [0 .. wCount - 1]
          

rfft :: [Complex Double] -> [Complex Double]
rfft = map conjugate . fft . map conjugate         

