{-# LANGUAGE Arrows #-}
module Effects where
import Euterpea


vibrato :: Rate -> Depth -> AudSF Double Double
vibrato rate depth =
    proc sin -> do
    vib   <- osc sineTable 0  -< rate
    sout  <- delayLine1 0.2   -< (sin,0.1+0.005*vib)
    outA -< sout