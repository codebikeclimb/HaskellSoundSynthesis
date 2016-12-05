{-# LANGUAGE Arrows #-}
module InstrumentList where

import Euterpea

-- SineWave generator 
  
sineTable :: Table
sineTable = tableSinesN 4096 [1]

--Constructing the InstrMap:

sawName, fluteName, bellName :: InstrumentName
sawName = CustomInstrument "Sawtooth wave"
fluteName = CustomInstrument "Flute"
bellName = CustomInstrument "Bell"

myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [(sawName, sawWave), (fluteName, fluteInstr),(bellName,bellInstr)]


-- SawWave Instrument
 
sawWave :: Instr (Mono AudRate)
sawWave dur pch vol params =
    let dur' = fromRational dur
        freq = apToHz pch
    in proc _ -> do
       osc1 <- osc sineTable 0 -< freq
       osc2 <- osc sineTable 0 -< freq*2
       osc3 <- osc sineTable 0 -< freq*3
       osc4 <- osc sineTable 0 -< freq*4
       osc5 <- osc sineTable 0 -< freq*5
       env1 <- envLineSeg [0, 1.0, 0, 0] [0.05,1,100] -< ()
       env2 <- envLineSeg [0, 0.5, 0, 0] [0.05,2,100] -< () 
       env3 <- envLineSeg [0, 0.33, 0, 0] [0.05,2,100] -< ()
       env4 <- envLineSeg [0, 0.25, 0, 0] [0.05,2,100] -< ()
       env5 <- envLineSeg [0, 0.2, 0, 0] [0.05,2,100] -< ()
       let partials = ((osc1*env1) + (osc2*env2) + (osc3*env3) + (osc4*env4) + (osc5*env5)) / 5
       rec saw <- delayLine (1/freq)   -< effect
           x   <- delayLine (1/freq/2) -< partials + saw*0.4
           effect <- filterLowPassBW -< (x-x*x*x + saw*0.4, 2000)
       outA -< 0.95 * partials * effect
       


-- SawWave Test

sawNote = writeWav "sawtoothNoteModded.wav" myInstrMap
    (tempo 0.5 $ transpose 12 $ instrument sawName (g 5 wn))
sawNotes = writeWav "sawNotesModded.wav" myInstrMap
    (tempo 0.4 $ instrument sawName mel1)
    
mel1 = toMusic1 $ line $ map ($en) [c 4, c 5, b 4, a 4, g 4, f 4, e 4, d 4, c 4]    
    
-- FLUTE Instrument

--f0  = flute 3 0.35 440 0.93 0.02 -- average breath
--f1  = flute 3 0.35 440 0.83 0.05 -- weak breath, soft note
--f2  = flute 3 0.35 440 0.53 0.04 -- very weak breath, no note

flute ::  Double -> Double -> Double -> Double -> Double 
          -> AudSF () Double
flute dur amp fqc press breath = proc _ -> do
     env1   <- envLineSeg  [0, 1.1*press, press, press, 0]  --envLineSeg has changed check it out in Euterpea2
                           [0.06, 0.2, dur-0.16, 0.02] -< ()
     env2   <- envLineSeg  [0, 1, 1, 0]
                           [0.01, dur-0.02, 0.01]       -< ()
     envib  <- envLineSeg  [0, 0, 1, 1] 
                           [0.5, 0.5, dur-1]            -< ()
     flow   <- noiseWhite 42    -< ()
     vib    <- osc sineTable 0  -< 5
     let  emb = breath*flow*env1 + env1 + vib*0.1*envib
     rec  flute  <- delayLine (1/fqc)    -< out
          x      <- delayLine (1/fqc/2)  -< emb + flute*0.4
          out    <- filterLowPassBW -< (x-x*x*x + flute*0.4, 2000)
     outA -< out*amp*env2
 
fluteInstr :: Instr (Mono AudRate)
fluteInstr dur pch vol params = 
     let freq = apToHz pch 
     in  flute (fromRational dur)(fromIntegral vol/127) freq 0.93 0.02 


-- Flute test     

flute1 = instrument fluteName $ 
     line [c 4 qn, e 4 qn, g 4 qn, g 2 qn]

     
test = writeWav "test.wav" myInstrMap flute1



-- BELL Instrument

bellInstr :: Instr (AudSF () Double)
bellInstr dur ap vol pfields = 
  let dur' = fromRational dur 
      f = apToHz ap
  in  proc () -> do
        x1 <- osc sineTable 0 -< f
        x2 <- osc sineTable 0 -< f*4.1
        x3 <- osc sineTable 0 -< f*6.05
        x4 <- osc sineTable 0 -< f*8.2
        env1 <- envLineSeg [1.0, 0.2, 0, 0] [1, 2, 100] -< ()
        env2 <- envLineSeg [0, 0.8, 0, 0] [0.05, 2, 100] -< ()
        env3 <- envLineSeg [0, 0.5, 0, 0] [0.08, 2, 100] -< ()
        env4 <- envLineSeg [0, 0.3, 0, 0] [0.015, 1, 100] -< ()
        envx1 <- envLineSeg [0,1,1,0] [0.0001*dur',0.9999*dur', 0.0001*dur'] -< ()
        envx2 <- envLineSeg [1,0.5,0.2,0,0] [0.05,0.2,3,100] -< ()
        let envs = envx2
            partials = ((x1*env1) + (x2*env2) + (x3*env3) + (x4*env4)) / 4
        outA -< 0.95 * envs * partials    


-- Bell Test

        
bellNote = writeWav "bellNote.wav" myInstrMap 
     (tempo 0.5 $ transpose 12 $ instrument bellName (g 5 wn))    
    