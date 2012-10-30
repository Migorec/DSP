module Noise where

import Signals
import Data.Complex
import Control.Monad
import System.Random
import Data.Random.Normal


uniformNoise :: Int -> Double -> IO [Complex Double]
uniformNoise n a =
    do replicateM n $ do r <- randomIO :: IO Double
                         if r > (5 / fromIntegral n)
                         then return 0
                         else do r <- randomIO :: IO Double
                                 return (r*a :+ 0)
                                 
normalNoise :: Int -> Double -> Double -> IO [Complex Double]
normalNoise n a s = 
    do replicateM n $ do r <- normalIO' (a,s)
                         return (r :+ 0)

applyNoise :: DSignal -> [Complex Double] -> DSignal
applyNoise (DSignal t0 dt l) noise = DSignal t0 dt $ zipWith (+) l noise 


{-
uniformNoise :: Double -> DSignal -> IO DSignal
uniformNoise a s = 
    do print n
       nind <- mapM (\v -> do r <- randomIO :: IO Double
                              if r > (5 / n)
                              then return v
                              else do r <- randomIO :: IO Double
                                      return (v + (r*a :+ 0))
                    ) $ ind s
       return s{ind = nind}
    where n = fromIntegral $ length $ ind s


normalNoise :: Double -> Double -> DSignal -> IO DSignal
normalNoise a s sig = 
    do nind <- mapM (\v -> do r <- normalIO' (a,s)
                              return $ v + (r :+ 0)
                    ) $ ind sig
       return sig{ind = nind}
-}       
