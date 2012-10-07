module Service where

rect :: (Ord a, Num a) => a -> a
rect x | x >= 0 && x <= 1 = 1
       | otherwise = 0
             
sinc :: (Eq a, Floating a) => a -> a
sinc 0 = 1
sinc x = sin x / x
