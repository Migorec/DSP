module Service (rect, sinc, oddEven) where

rect :: (Ord a, Num a) => a -> a
rect x | x >= 0 && x <= 1 = 1
       | otherwise = 0
             
sinc :: (Eq a, Floating a) => a -> a
sinc 0 = 1
sinc x = sin x / x


--кривая вспомогательная функция
myUnzip :: [[a]] -> ([a],[a])
myUnzip = foldr f ([],[])
    where f [x,y] (xs,ys) = (x:xs, y:ys)
          f [x] (xs,ys) = (x:xs, ys)

oddEven :: [a] -> ([a], [a])
oddEven l = myUnzip $ oePairs l
    where oePairs [] = []
          oePairs [x] = [[x]]
          oePairs (x1:x2:xs) = [x1,x2]:(oePairs xs) 
