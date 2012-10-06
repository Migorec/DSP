module Signals where

import Service

class Signal a where
    eval :: a -> Double -> Double


data CSignal = Impuls  { t0 :: Double,
                         amp :: Double } |
               SImpuls { t :: Double,
                         amp :: Double } |
               Gaus    { sigma :: Double,
                         amp :: Double } 
                                      
instance Signal CSignal where
    eval (Impuls t a) x  | abs (x-t) < 0.00001 = a
                         | otherwise = 0
    eval (SImpuls t a) x = a * rect ((x - t)/t)
    eval (Gaus s a) x    = a * exp (-x*x/s/s) 
                     
data DSignal = DSignal {dt  :: Double,
                        ind ::[(Double, Double)]
                       } deriving Show

instance Signal DSignal where
    eval (DSignal dt ind) x = sum $ map (\(tk,uk) -> uk*sinc (2*pi*f*(x - k tk / 2 / f))) ind
        where f = 1/2/dt
              k x = x/dt 
              
discretize :: CSignal -> Double -> Double -> Double -> DSignal
discretize sig a b dx = DSignal dx $ map (\x -> (x,eval sig x)) [a,a+dx .. b] 
