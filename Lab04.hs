module Lab04 where

import Signals
import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.WXT
import Noise
import Filter.Convolution
import Filter.IIR
import Filter.FIR
import Fourier




main = do
    let sig = Gaus 0 1 5
        u = discretize sig (-3) 3 0.05
    nu <-  uniformNoise 3 u--normalNoise 3 0.2 u -- 
     
    let i = ind nu
        --i = zipWith (*) di $ map (\t -> exp (0 :+ pi*(t/0.1))) [-3, -3 + 0.1 ..  3]
        n = length i
        --rnu =  highBatterworth nu 0.1
        rnu = nu{ind = rfft $ zipWith (*) (fft  i) ( gaus 0.5 (1/(dt nu)/fromIntegral n))}
--    print rnu
    let
        dftu = zip [-3,-3+0.05 .. 3] $ map magnitude $ ind nu
        rdftu = zip [-3,-3+0.05 .. 3] $ map magnitude $ ind rnu
    --    rdftu = map (\x -> (x,evalA rnu x)) [-3,-3+0.01 .. 3]  
        
        st1 = defaultStyle {lineSpec = CustomStyle [LineTitle "Convolution", LineWidth 3.0]}
        st2 = defaultStyle {lineSpec = CustomStyle [LineTitle "Convolution", LineWidth 2.0]}
    
    
    plotPathsStyle [terminal cons] [(st1,dftu),(st2,rdftu)] 
