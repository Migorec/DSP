

import Signals
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.WXT


lab01 :: CSignal -> Double -> Double -> Double -> Double -> IO ()
lab01 sig a b dx dt = 
    do let xs = [a, a+dx .. b]
           cu = map (\x -> (x, eval sig x)) xs
           dsig = discretize sig a b dt
           du = map (\x -> (x, eval dsig x)) xs
           
           st1 = defaultStyle {lineSpec = CustomStyle [LineTitle "Source signal", LineWidth 5.0]}
           st2 = defaultStyle {lineSpec = CustomStyle [LineTitle "Restored signal", LineWidth 2.0]}
       plotPathsStyle [terminal cons] [(st1,cu),(st2,du)] 
           
