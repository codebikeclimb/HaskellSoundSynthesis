{-# LANGUAGE Arrows #-}
module Effects where
import Euterpea


sineTable :: Table
sineTable = tableSinesN 4096 [1]

vibrato :: Double -> Double -> AudSF Double Double
vibrato rate depth =
    proc sin -> do
    vib   <- osc sineTable 0  -< rate
    sout  <- delayLine1 0.2   -< (sin,0.1+0.005*vib)
    outA -< sout

tModVib = outFile "modvib.wav" 6 $
                  Double 440 >>> osc sineTable 0 >>> vibrato 5 0.00
    
echo :: AudSF Double Double
echo = proc s -> do
         rec fb  <- delayLine 0.5 -< s + 0.7*fb
         outA -< fb/3    