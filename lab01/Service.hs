module Service where

rect :: Double -> Double
rect x | x >= 0 && x <= 1 = 1
       | otherwise = 0
             
sinc :: Double -> Double
sinc 0 = 1
sinc x = sin x / x
