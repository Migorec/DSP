module Signals where

import Data.Complex
import Service

class Signal a where
    eval :: a -> Double -> Complex Double
    evalA :: a -> Double -> Double
    
    evalA s x =  magnitude $ eval s x


data CSignal = Impuls  { t :: Double,
                         amp :: Double } |
               SImpuls { t :: Double,
                         amp :: Double } |
               Gaus    { sigma :: Double,
                         amp :: Double } 
                                      
instance Signal CSignal where
    eval (Impuls t a) x  | abs (x-t) < 0.00001 = a :+ 0
                         | otherwise = 0
    eval (SImpuls t a) x = a * rect ((x - t)/t) :+ 0
    eval (Gaus s a) x    = a * exp (-x*x/s/s)  :+ 0
                     
data DSignal = DSignal {dt  :: Double,
                        t0 :: Double,
                        ind ::[Complex Double]
                       } deriving Show

instance Signal DSignal where
    eval (DSignal t0 dt ind) x =
         sum $ zipWith (\u t -> u * sinc (2*(pi :+ 0)*f*((x :+ 0) - k t / 2 / f))) 
                       ind [t0, t0 + dt ..] 
        where f = 1/2/dt :+ 0
              k x = x/dt :+ 0
              
discretize :: CSignal -> Double -> Double -> Double -> DSignal
discretize sig a b dx = DSignal a dx $ map (eval sig) [a,a+dx .. b] 
