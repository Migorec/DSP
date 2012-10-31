module Lab03 where

import Signals
import Fourier
import Data.Complex
import Filter.Convolution
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.WXT
import Fourier
import Tikhonov

lab06 = do let a = -10
               b = 10
               n = 128
               dt = (b - a ) / fromIntegral (n - 1)
               u1 = discretize (Gaus 0 (sqrt 2) 1) a b dt
               u2 = discretize (Gaus 0 1 1) a b dt
               --ffu1 = fft $ ind u1
               --ffu2 = fft $ ind u2
               ffh = tikhonov dt (ind u1) (ind u1)
               h = u1{ind = ffh}
               gu1 = map (\x -> (x, evalA u1 x)) [a,a+dt .. b]
               gu2 = map (\x -> (x, evalA u2 x)) [a,a+dt .. b]
               gh = map (\x -> (x, evalA h x)) [a,a+dt .. b]
               st1 = defaultStyle {lineSpec = CustomStyle [LineTitle "u1", LineWidth 3.0]}
               st2 = defaultStyle {lineSpec = CustomStyle [LineTitle "u2", LineWidth 2.0]}
               st3 = defaultStyle {lineSpec = CustomStyle [LineTitle "H", LineWidth 2.0]}
           plotPathsStyle [terminal cons] [(st1,gu1),(st2,gu2),(st3,gh)]

