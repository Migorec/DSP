module Lab04 where

import Signals
import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.WXT
import Noise
import Filter.Convolution
import qualified Filter.IIR as IIR
import qualified Filter.FIR as FIR
import Filter.Weiner
import Fourier


lab04 :: CSignal -> 
         Double -> 
         Double -> 
         Int -> 
         (Int -> IO [Complex Double]) -> 
         (DSignal -> DSignal) -> 
         IO ()
lab04 csig a b n no fi =
    do let dt = (b - a) / fromIntegral (n - 1)
           u = discretize csig a b dt
       noise <- no n
       let nu = applyNoise u noise 
           fu = fi nu
           gnu = map (\x -> (x, evalA nu x)) [a,a+dt .. b]
           gfu = map (\x -> (x, evalA fu x)) [a,a+dt .. b]
           st1 = defaultStyle {lineSpec = CustomStyle [LineTitle "with noise", LineWidth 3.0]}
           st2 = defaultStyle {lineSpec = CustomStyle [LineTitle "filtered", LineWidth 2.0]}
       plotPathsStyle [terminal cons] [(st1,gnu),(st2,gfu)]



lab05 :: CSignal -> 
         Double -> 
         Double -> 
         Int -> 
         (Int -> IO [Complex Double]) -> 
         IO ()
lab05 csig a b n no =
    do let dt = (b - a) / fromIntegral (n - 1)
           u = discretize csig a b dt
       noise <- no n
       let nu = applyNoise u noise
           fu = weiner  nuise nu
           --fu = fi nu
           gnu = map (\x -> (x, evalA nu x)) [a,a+dt .. b]
           gfu = map (\x -> (x, evalA fu x)) [a,a+dt .. b]
           st1 = defaultStyle {lineSpec = CustomStyle [LineTitle "with noise", LineWidth 3.0]}
           st2 = defaultStyle {lineSpec = CustomStyle [LineTitle "filtered", LineWidth 2.0]}
       plotPathsStyle [terminal cons] [(st1,gnu),(st2,gfu)]
        
        
main = lab05 (Gaus 0 1 5) (-3) 3 128 (uniformNoise 3)  --(weiner (uniformNoise 3))
  
