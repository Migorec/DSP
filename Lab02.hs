module Lab02 where

import Signals
import Fourier
import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.WXT
import System.CPUTime
import Control.Exception
import Control.DeepSeq
import Control.Monad


lab02 :: CSignal -> Double -> Double -> Int -> IO ()
lab02 sig a b n = 
    do let h = (b - a) / fromIntegral (n - 1)
       ds@(DSignal t0 dt l) <- do (DSignal t0 dt l) <- evaluate $ discretize sig a b h
                                  x <- evaluate $ DSignal t0 dt $ zipWith (*) l $ map (\t -> exp (0 :+ pi*t)) [a, a + h ..  b]
                                  rnf x `seq` return x  
       
       dftt0 <- getCPUTime              
       dftr <- do x <- evaluate (DSignal 0 (1/ fromIntegral n / dt) $ dft l)
                  rnf x `seq` return x
       dftt <- getCPUTime 
       
       fftt0 <- getCPUTime 
       fftr <- do x <- evaluate (DSignal 0 (1/ fromIntegral n / dt) $ fft l)
                  rnf x `seq` return x
       fftt <- getCPUTime 
       
       let df = 1 / dt / fromIntegral n;
           dftu = zip [0,0+dt .. 0 + dt * fromIntegral n] $ map magnitude $ ind dftr
           fftu = zip [0,0+dt .. 0 + dt * fromIntegral n] $ map magnitude $ ind fftr
      {- 
       let du = map (\x -> (x, evalA fs x)) [-1,-0.99 .. 10]
       du1 = map (\x -> (x, evalA fs1 x)) [-1,-0.99 .. 10]
      -}
           st1 = defaultStyle {lineSpec = CustomStyle [LineTitle ("dft (" ++ show (dftt - dftt0) ++ ")"), LineWidth 5.0]}
           st2 = defaultStyle {lineSpec = CustomStyle [LineTitle ("fft (" ++ show (fftt - fftt0) ++ ")"), LineWidth 2.0]}
       plotPathsStyle [terminal cons] [(st1,dftu), (st2, fftu)] 
