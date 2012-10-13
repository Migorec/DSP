module Lab03 where

import Signals
import Fourier
import Data.Complex
import Filter.Convolution
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.WXT


lab03 :: CSignal -> CSignal -> Double -> Double -> Int -> IO ()
lab03 u h a b n = 
    do let dt = (b - a) / fromIntegral (n - 1)
           xs = [a, a+dt .. b]
           du = discretize u a b dt
           dh = discretize h a b dt
           --du = DSignal a dt $ zipWith (*) (ind $ discretize u a b dt) $ map (\t -> exp (0 :+ pi*t)) xs
           --dh = DSignal a dt $ zipWith (*) (ind $ discretize h a b dt) $ map (\t -> exp (0 :+ pi*t)) xs
           conv = DSignal a dt $ linearConvolution (ind du) (ind dh)
           cConv = map (\x -> (x, evalA conv x)) [a, a+dt .. b + (b -a)]
           
           st1 = defaultStyle {lineSpec = CustomStyle [LineTitle "Convolution", LineWidth 3.0]}
       plotPathsStyle [terminal cons] [(st1,cConv)] 
