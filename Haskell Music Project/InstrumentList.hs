{-# LANGUAGE Arrows #-}
module InstrumentList where

--import Effects
import Euterpea

-- Export effects



-- Signal generators
  
sineTable :: Table
sineTable = tableSinesN 4096 [1]


--Constructing the InstrMap:

sineName, sawName, sqName, triName, fluteName, bellName, wavName, stringName :: InstrumentName
sawName = CustomInstrument "Sawtooth wave"
sqName = CustomInstrument "Square Wave"
triName = CustomInstrument "Triangle Wave"
fluteName = CustomInstrument "Flute"
bellName = CustomInstrument "Bell"
wavName = CustomInstrument "Wave"
stringName = CustomInstrument "String"
sineName = CustomInstrument "Sine"

myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [(sineName, sineInstr), (sawName, sawWave), (sqName,squWave), (triName,triWave), (fluteName, fluteInstr),(bellName,bellInstr)]

-- Melody construction samples
mel2 = note qn (C,4) :+: note qn (E,4) :+: 
     note qn (G,4) :+: note qn (C,5)

mel3 = c 4 qn :+: e 4 qn :+: g 4 qn :+: c 5 qn

mel4 = instrument sawName $ line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]


sonataInC :: Music Pitch
sonataInC = line [c 5 wn, e 5 hn, g 5 hn, b 4 dhn, c 5 en, d 5 en, c 5 hn, rest hn,
             a 5 wn, g 5 hn, c 6 hn, g 5 hn, f 5 en, g 5 en, e 5 en, f 5 en, e 5 hn, rest hn, 
             a 4 qn, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en,
             g 4 qn, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en,
             f 4 qn, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en,
             e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en,
             e 4 qn, f 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, 
             d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en, e 4 en,
             d 4 qn, e 4 en, f 4 en, g 4 en, a 4 en, b 4 en, cs 5 en,
             d 5 en, a 4 en, b 4 en, cs 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             a 5 en, b 5 en, c 6 en, b 5 en, a 5 en, g 5 en, f 5 en, e 5 en,
             f 5 en, g 5 en, a 5 en, g 5 en, f 5 en, e 5 en, d 5 en, c 5 en,
             b 4 qn, g 5 qn, e 5 qn, c 5 qn, d 5 qn, g 5 qn, e 5 qn, c 5 qn,
             d 5 hn, g 5 hn, g 4 hn, rest hn,
             fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en,
             f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en,
             g 5 qn, e 5 qn, c 5 dhn, d 5 en, e 5 en, d 5 qn, c 5 qn,
             c 5 dqn, b 4 en, b 4 hn, rest wn, g 5 qn, e 5 qn, c 5 dhn,
             d 5 en, e 5 en, d 5 qn, c 5 qn, c 5 dqn, b 4 en, b 4 hn, rest wn,
             g 5 en, e 3 en,g 3 en, c 4 en, e 4 en, g 5 en, e 5 en, c 5 en,
             a 4 en, f 3 en, a 3 en, c 4 en, f 4 en, a 4 en, c 5 en, a 4 en,
             f 5 en, d 3 en, f 3 en, b 3 en, d 4 en, f 5 en, d 5 en, b 4 en,
             g 4 en, e 3 en, g 3 en, b 3 en, e 4 en, g 4 en, b 4 en, g 4 en,
             e 5 en, c 4 en, e 4 en, a 4 en, c 5 en, e 5 en, c 5 en, a 4 en,
             f 4 en, d 4 en, f 4 en, a 4 en, d 5 en, f 4 en, a 4 en, f 4 en,
             d 6 en, b 3 en, d 4 en, g 4 en, b 4 en, d 6 en, b 5 en, g 5 en,
             e 5 en, c 4 en, e 4 en, g 4 en, c 5 en, c 6 en, g 5 en, e 5 en,
             d 5 wn, d 5 hn, d 5 hn, a 5 wn, a 5 hn, a 5 hn, g 5 qn, a 5 en,
             b 5 en, c 6 en, d 6 en, e 6 en, d 6 en, c 6 en, b 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, c 5 en, d 5 en, c 5 hn, c 5 en, g 4 en,
             c 5 en, e 5 en, g 5 en, e 5 en, c 5 en, e 5 en, f 5 en, d 5 en,
             b 4 en, d 5 en, c 5 hn, c 4 en, g 3 en, c 4 en, e 4 en, g 4 en,
             e 4 en, c 4 en, e 4 en, f 4 en, d 4 en, b 3 en, d 4 en, c 4 hn,
             c 5 hn, c 4 hn]

             
             
-- Sine Instrument

sineInstr :: Instr (Mono AudRate)
sineInstr dur pch vol params = 
    let freq = apToHz pch 
    in  proc _ -> do
        y <- osc sineTable 0 -< freq     
        returnA -< y             
             
        
        
sineNotes = writeWav "sineWaveVib.wav" myInstrMap sineTest
                
sineTest = instrument sineName $ 
         line [c 4 qn, e 4 qn, g 4 qn, c 5 qn] 

         
         --vb <- osc sineTable 0 -< 6
         --freq + vb*5
         --rec fb  <- delayLine 0.5 -< y + 0.7*fb
         --outA -< y + fb/3
         
-- Karplus-Strong String Synthesis
bnoise :: AudSF () Double
bnoise = proc () -> do
    env1 <- envLineSeg [1,1,0,0] [0.01, 0.000001, 9.8] -< ()
    noise <- noiseWhite 44 -< ()
    outA -< env1 * noise

stringInstr :: AudSF Double Double
stringInstr = proc input -> do
        rec delay <- delayLine 0.01 -< (flt+input)
            flt <- filterLowPass -< (delay, 2000)
        outA -< flt + input
        
stringNote = outFile "test$.wav" 5 $ bnoise >>> stringInstr
-- Triangle Wave
triWave :: Instr (Mono AudRate)
triWave dur pch vol params =
    let dur' = fromRational dur
        freq = apToHz pch
    in proc _ -> do
       osc1 <- osc sineTable 0 -< freq
       osc2 <- osc sineTable 0 -< freq*3
       osc3 <- osc sineTable 0 -< freq*5
       env1 <- envLineSeg [0, 1.0, 0, 0] [0.05,1,100] -< ()
       env2 <- envLineSeg [0, 0.11, 0, 0] [0.05,2,100] -< () 
       env3 <- envLineSeg [0, 0.0004, 0, 0] [0.05,2,100] -< ()
       let partials = ((osc1*env1) + (osc2*env2) + (osc3*env3)) / 3
       rec square <- delayLine (1/freq)   -< effect
           x   <- delayLine (1/freq/2) -< partials + square*0.4
           effect <- filterLowPassBW -< (x-x*x*x + square*0.4, 2000)
       outA -< 0.95 * partials * effect

triNotes = writeWav "TriangleWaves.wav" myInstrMap
    (tempo 0.4 $ instrument triName mel1)

-- SquareWave
squWave :: Instr (Mono AudRate)
squWave dur pch vol params =
    let dur' = fromRational dur
        freq = apToHz pch
    in proc _ -> do
       osc1 <- osc sineTable 0 -< freq
       osc2 <- osc sineTable 0 -< freq*3
       osc3 <- osc sineTable 0 -< freq*5
       osc4 <- osc sineTable 0 -< freq*7
       osc5 <- osc sineTable 0 -< freq*9
       env1 <- envLineSeg [0, 1.0, 0, 0] [0.05,1,100] -< ()
       env2 <- envLineSeg [0, 0.33, 0, 0] [0.05,2,100] -< () 
       env3 <- envLineSeg [0, 0.2, 0, 0] [0.05,2,100] -< ()
       env4 <- envLineSeg [0, 0.143, 0, 0] [0.05,2,100] -< ()
       env5 <- envLineSeg [0, 0.11, 0, 0] [0.05,2,100] -< ()
       let partials = ((osc1*env1) + (osc2*env2) + (osc3*env3) + (osc4*env4) + (osc5*env5)) / 5
       rec square <- delayLine (1/freq)   -< effect
           x   <- delayLine (1/freq/2) -< partials + square*0.4
           effect <- filterLowPassBW -< (x-x*x*x + square*0.4, 2000)
       outA -< 0.95 * partials * effect
      
-- SquareWave Test      
sqNotes = writeWav "SquareWaves.wav" myInstrMap
    (tempo 0.4 $ instrument sqName mel1)
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
     line [c 4 qn, e 4 qn, g 4 qn]
     
flute2 = instrument fluteName $ sonataInC     

-- MORE STUFF COLLAPSED     
test = writeWav "flute.wav" myInstrMap flute1


-- BELL Instrument

bellInstr :: Instr (AudSF () Double)
bellInstr dur ap vol pfields = 
  let dur' = fromRational dur 
      f = apToHz ap
  in  proc () -> do
        vb <- osc sineTable 0 -< 6
        x1 <- osc sineTable 0 -< f + vb*0.5
        x2 <- osc sineTable 0 -< f*4.1 + vb*10
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
     (tempo 0.9 $ transpose 12 $ instrument bellName sonataInC)    
     