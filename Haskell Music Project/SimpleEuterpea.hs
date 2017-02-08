module SimpleEuterpea(module SimpleEuterpea, E.InstrumentName(..), E.play) where

import qualified Euterpea as E
import System.Random
import qualified Data.List
import InstrumentList

writeMidi file comp = writeWav (file ++ ".wav") myInstrMap comp 

--- Need louder and softer mofdifiers

type Music = E.Music E.Pitch

type Duration = Double -- Should be rational

-- Pitches

r :: Music
r = E.wnr

empty :: Music
empty = E.rest 0

-- Convert a pitch number to a midi note with default attributes
pitchToMusic p = makeSf (E.note E.wn (E.pitch p))
-- Names used for the 12 scale degrees - note this chooses sharp over flat for enharmonic notes.
degreeNames = ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"] 
-- get the name of a pitch number.
pitchToName p | p < 23 || p > 120 = "Not a pitch: " ++ show p
              | otherwise = degreeNames !! (p `mod` 12) ++ show ((p - 12) `div` 12)
              
-- Basic library of note names
cf1 = pitchToMusic 23
c1 = pitchToMusic 24
cs1 = pitchToMusic 25
df1 = pitchToMusic 25
d1 = pitchToMusic 26
ds1 = pitchToMusic 27
ef1 = pitchToMusic 27
e1 = pitchToMusic 28
ff1 = pitchToMusic 28
es1 = pitchToMusic 29
f1 = pitchToMusic 29
fs1 = pitchToMusic 30
gf1 = pitchToMusic 30
g1 = pitchToMusic 31
gs1 = pitchToMusic 32
af1 = pitchToMusic 32
a1 = pitchToMusic 33
as1 = pitchToMusic 34
bf1 = pitchToMusic 34
b1 = pitchToMusic 35
bs1 = pitchToMusic 36
cf2 = pitchToMusic 35
c2 = pitchToMusic 36
cs2 = pitchToMusic 37
df2 = pitchToMusic 37
d2 = pitchToMusic 38
ds2 = pitchToMusic 39
ef2 = pitchToMusic 39
e2 = pitchToMusic 40
ff2 = pitchToMusic 40
es2 = pitchToMusic 41
f2 = pitchToMusic 41
fs2 = pitchToMusic 42
gf2 = pitchToMusic 42
g2 = pitchToMusic 43
gs2 = pitchToMusic 44
af2 = pitchToMusic 44
a2 = pitchToMusic 45
as2 = pitchToMusic 46
bf2 = pitchToMusic 46
b2 = pitchToMusic 47
bs2 = pitchToMusic 48
cf3 = pitchToMusic 47
c3 = pitchToMusic 48
cs3 = pitchToMusic 49
df3 = pitchToMusic 49
d3 = pitchToMusic 50
ds3 = pitchToMusic 51
ef3 = pitchToMusic 51
e3 = pitchToMusic 52
ff3 = pitchToMusic 52
es3 = pitchToMusic 53
f3 = pitchToMusic 53
fs3 = pitchToMusic 54
gf3 = pitchToMusic 54
g3 = pitchToMusic 55
gs3 = pitchToMusic 56
af3 = pitchToMusic 56
a3 = pitchToMusic 57
as3 = pitchToMusic 58
bf3 = pitchToMusic 58
b3 = pitchToMusic 59
bs3 = pitchToMusic 60
cf4 = pitchToMusic 59
c4 = pitchToMusic 60
cs4 = pitchToMusic 61
df4 = pitchToMusic 61
d4 = pitchToMusic 62
ds4 = pitchToMusic 63
ef4 = pitchToMusic 63
e4 = pitchToMusic 64
ff4 = pitchToMusic 64
es4 = pitchToMusic 65
f4 = pitchToMusic 65
fs4 = pitchToMusic 66
gf4 = pitchToMusic 66
g4 = pitchToMusic 67
gs4 = pitchToMusic 68
af4 = pitchToMusic 68
a4 = pitchToMusic 69
as4 = pitchToMusic 70
bf4 = pitchToMusic 70
b4 = pitchToMusic 71
bs4 = pitchToMusic 72
cf5 = pitchToMusic 71
c5 = pitchToMusic 72
cs5 = pitchToMusic 73
df5 = pitchToMusic 73
d5 = pitchToMusic 74
ds5 = pitchToMusic 75
ef5 = pitchToMusic 75
e5 = pitchToMusic 76
ff5 = pitchToMusic 76
es5 = pitchToMusic 77
f5 = pitchToMusic 77
fs5 = pitchToMusic 78
gf5 = pitchToMusic 78
g5 = pitchToMusic 79
gs5 = pitchToMusic 80
af5 = pitchToMusic 80
a5 = pitchToMusic 81
as5 = pitchToMusic 82
bf5 = pitchToMusic 82
b5 = pitchToMusic 83
bs5 = pitchToMusic 84
cf6 = pitchToMusic 83
c6 = pitchToMusic 84
cs6 = pitchToMusic 85
df6 = pitchToMusic 85
d6 = pitchToMusic 86
ds6 = pitchToMusic 87
ef6 = pitchToMusic 87
e6 = pitchToMusic 88
ff6 = pitchToMusic 88
es6 = pitchToMusic 89
f6 = pitchToMusic 89
fs6 = pitchToMusic 90
gf6 = pitchToMusic 90
g6 = pitchToMusic 91
gs6 = pitchToMusic 92
af6 = pitchToMusic 92
a6 = pitchToMusic 93
as6 = pitchToMusic 94
bf6 = pitchToMusic 94
b6 = pitchToMusic 95
bs6 = pitchToMusic 96
cf7 = pitchToMusic 95
c7 = pitchToMusic 96
cs7 = pitchToMusic 97
df7 = pitchToMusic 97
d7 = pitchToMusic 98
ds7 = pitchToMusic 99
ef7 = pitchToMusic 99
e7 = pitchToMusic 100
ff7 = pitchToMusic 100
es7 = pitchToMusic 101
f7 = pitchToMusic 101
fs7 = pitchToMusic 102
gf7 = pitchToMusic 102
g7 = pitchToMusic 103
gs7 = pitchToMusic 104
af7 = pitchToMusic 104
a7 = pitchToMusic 105
as7 = pitchToMusic 106
bf7 = pitchToMusic 106
b7 = pitchToMusic 107
bs7 = pitchToMusic 108
cf8 = pitchToMusic 107
c8 = pitchToMusic 108
cs8 = pitchToMusic 109
df8 = pitchToMusic 109
d8 = pitchToMusic 110
ds8 = pitchToMusic 111
ef8 = pitchToMusic 111
e8 = pitchToMusic 112
ff8 = pitchToMusic 112
es8 = pitchToMusic 113
f8 = pitchToMusic 113
fs8 = pitchToMusic 114
gf8 = pitchToMusic 114
g8 = pitchToMusic 115
gs8 = pitchToMusic 116
af8 = pitchToMusic 116
a8 = pitchToMusic 117
as8 = pitchToMusic 118
bf8 = pitchToMusic 118
b8 = pitchToMusic 119
bs8 = pitchToMusic 120


-- Transposition 

up, down :: Int -> Music -> Music
up n m = E.transpose n m

down n m = E.transpose (-n) m


-- Duration changes

faster n m = E.tempo n m
slower n m = E.tempo (1/n) m

dw, w, dh, h, dq, q, de, e, ds, s, dot, ddot :: Music -> Music
dw = dot . w
ddw = ddot . w
dddw = dddot . w
w = id
h = slower (1/2)
dh = dot . h
ddh = ddot . h
dddh = dddot . h
q = slower (1/4)
dq = dot . q
ddq = ddot . q
dddq = dddot . q
e = slower (1/8)
de = dot . e
dde = ddot . e
ddde = dddot . e
s = slower (1/16)
ds = dot . s
dds = ddot . s
ddds = dddot . s
t = slower (1/32)
dt = dot . t
ddt = ddot . t
dddt = dddot . t
dot = slower (3/2)
ddot = slower (7/4)
dddot = slower (15/16)
triplet = slower (2/3)




-- Interval names

per1 = 0
aug1 = 1
dim2 = 0
min2 = 1
maj2 = 2
aug2 = 3
dim3 = 2
min3 = 3
maj3 = 4
aug3 = 5
dim4 = 4
per4 = 5
aug4 = 6
dim5 = 6
per5 = 7
aug5 = 8
dim6 = 7
min6 = 8
maj6 = 9
aug6 = 10
dim7 = 9
min7 = 10
maj7 = 11
aug7 = 12
dim8 = 11
per8 = 12

-- Instruments


sawWave = E.instrument sawName
piano = E.instrument E.AcousticGrandPiano
honkyTonkPiano = E.instrument E.HonkyTonkPiano
chorusedPiano = E.instrument E.ChorusedPiano
harpsichord = E.instrument E.Harpsichord
clavinet = E.instrument E.Clavinet
celesta = E.instrument E.Celesta
glockenspiel = E.instrument E.Glockenspiel
musicBox = E.instrument E.MusicBox
vibraphone = E.instrument E.Vibraphone
marimba = E.instrument E.Marimba
xylophone = E.instrument E.Xylophone
tubularBells = E.instrument E.TubularBells
dulcimer = E.instrument E.Dulcimer
hammondOrgan = E.instrument E.HammondOrgan
percussiveOrgan = E.instrument E.PercussiveOrgan
rockOrgan = E.instrument E.RockOrgan
churchOrgan = E.instrument E.ChurchOrgan
reedOrgan = E.instrument E.ReedOrgan
accordion = E.instrument E.Accordion
harmonica = E.instrument E.Harmonica
tangoAccordion = E.instrument E.TangoAccordion
acousticGuitarNylon = E.instrument E.AcousticGuitarNylon
acousticGuitarSteel = E.instrument E.AcousticGuitarSteel
electricGuitarJazz = E.instrument E.ElectricGuitarJazz
electricGuitarClean = E.instrument E.ElectricGuitarClean
electricGuitarMuted = E.instrument E.ElectricGuitarMuted
overdrivenGuitar = E.instrument E.OverdrivenGuitar
distortionGuitar = E.instrument E.DistortionGuitar
guitarHarmonics = E.instrument E.GuitarHarmonics
acousticBass = E.instrument E.AcousticBass
electricBassFingered = E.instrument E.ElectricBassFingered
electricBassPicked = E.instrument E.ElectricBassPicked
fretlessBass = E.instrument E.FretlessBass
slapBass1 = E.instrument E.SlapBass1
slapBass2 = E.instrument E.SlapBass2
synthBass1 = E.instrument E.SynthBass1
synthBass2 = E.instrument E.SynthBass2
violin = E.instrument E.Violin
viola = E.instrument E.Viola
cello = E.instrument E.Cello
contrabass = E.instrument E.Contrabass
tremoloStrings = E.instrument E.TremoloStrings
pizzicatoStrings = E.instrument E.PizzicatoStrings
orchestralHarp = E.instrument E.OrchestralHarp
timpani = E.instrument E.Timpani
stringEnsemble1 = E.instrument E.StringEnsemble1
stringEnsemble2 = E.instrument E.StringEnsemble2
synthStrings1 = E.instrument E.SynthStrings1
synthStrings2 = E.instrument E.SynthStrings2
choirAahs = E.instrument E.ChoirAahs
voiceOohs = E.instrument E.VoiceOohs
synthVoice = E.instrument E.SynthVoice
orchestraHit = E.instrument E.OrchestraHit
trumpet = E.instrument E.Trumpet
trombone = E.instrument E.Trombone
tuba = E.instrument E.Tuba
mutedTrumpet = E.instrument E.MutedTrumpet
frenchHorn = E.instrument E.FrenchHorn
brassSection = E.instrument E.BrassSection
synthBrass1 = E.instrument E.SynthBrass1
synthBrass2 = E.instrument E.SynthBrass2
sopranoSax = E.instrument E.SopranoSax
altoSax = E.instrument E.AltoSax
tenorSax = E.instrument E.TenorSax
baritoneSax = E.instrument E.BaritoneSax
oboe = E.instrument E.Oboe
bassoon = E.instrument E.Bassoon
englishHorn = E.instrument E.EnglishHorn
clarinet = E.instrument E.Clarinet
piccolo = E.instrument E.Piccolo
flute = E.instrument E.Flute
recorder = E.instrument E.Recorder
panFlute = E.instrument E.PanFlute
blownBottle = E.instrument E.BlownBottle
shakuhachi = E.instrument E.Shakuhachi
whistle = E.instrument E.Whistle
ocarina = E.instrument E.Ocarina
sitar = E.instrument E.Sitar
banjo = E.instrument E.Banjo
shamisen = E.instrument E.Shamisen
koto = E.instrument E.Koto
kalimba = E.instrument E.Kalimba
bagpipe = E.instrument E.Bagpipe
fiddle = E.instrument E.Fiddle
shanai = E.instrument E.Shanai
tinkleBell = E.instrument E.TinkleBell
agogo = E.instrument E.Agogo
steelDrums = E.instrument E.SteelDrums
woodblock = E.instrument E.Woodblock
taikoDrum = E.instrument E.TaikoDrum
melodicDrum = E.instrument E.MelodicDrum
synthDrum = E.instrument E.SynthDrum
reverseCymbal = E.instrument E.ReverseCymbal

-- Percussion
simplePerc n = makeSf $ E.instrument E.Percussion (E.perc n 1)

acousticBassDrum = simplePerc E.AcousticBassDrum
bassDrum1 = simplePerc E.BassDrum1
sideStick = simplePerc E.SideStick
acousticSnare = simplePerc E.AcousticSnare
handClap = simplePerc E.HandClap
electricSnare = simplePerc E.ElectricSnare
lowFloorTom = simplePerc E.LowFloorTom
closedHiHat = simplePerc E.ClosedHiHat
highFloorTom = simplePerc E.HighFloorTom
pedalHiHat = simplePerc E.PedalHiHat
lowTom = simplePerc E.LowTom
openHiHat = simplePerc E.OpenHiHat
lowMidTom = simplePerc E.LowMidTom
hiMidTom = simplePerc E.HiMidTom
crashCymbal1 = simplePerc E.CrashCymbal1
highTom = simplePerc E.HighTom
rideCymbal1 = simplePerc E.RideCymbal1
chineseCymbal = simplePerc E.ChineseCymbal
rideBell = simplePerc E.RideBell
tambourine = simplePerc E.Tambourine
splashCymbal = simplePerc E.SplashCymbal
cowbell = simplePerc E.Cowbell
crashCymbal2 = simplePerc E.CrashCymbal2
vibraslap = simplePerc E.Vibraslap
rideCymbal2 = simplePerc E.RideCymbal2
hiBongo = simplePerc E.HiBongo
lowBongo = simplePerc E.LowBongo
muteHiConga = simplePerc E.MuteHiConga
openHiConga = simplePerc E.OpenHiConga
lowConga = simplePerc E.LowConga
highTimbale = simplePerc E.HighTimbale
lowTimbale = simplePerc E.LowTimbale
highAgogo = simplePerc E.HighAgogo
lowAgogo = simplePerc E.LowAgogo
cabasa = simplePerc E.Cabasa
maracas = simplePerc E.Maracas
shortWhistle = simplePerc E.ShortWhistle
longWhistle = simplePerc E.LongWhistle
shortGuiro = simplePerc E.ShortGuiro
longGuiro = simplePerc E.LongGuiro
claves = simplePerc E.Claves
hiWoodBlock = simplePerc E.HiWoodBlock
lowWoodBlock = simplePerc E.LowWoodBlock
muteCuica = simplePerc E.MuteCuica
openCuica = simplePerc E.OpenCuica
muteTriangle = simplePerc E.MuteTriangle
openTriangle = simplePerc E.OpenTriangle

-- rhythm takes a string composed of the following:
-- space - rest
-- * - strike
-- > - accented strike
-- @ - very accented strike
-- _ - ignored (used to make things line up)
-- {.....} - triplet time


rhythm :: String -> Music -> Music
rhythm str note = rhythm1 str
  where
    rhythm1 :: String -> Music
    rhythm1 "" = empty
    rhythm1 ('_':s) = rhythm1 s   -- Ignore _
    rhythm1 (' ':s) = E.rest (E.dur note) + rhythm1 s
    rhythm1 ('{':s) = let (s1, s2) = span (/= '}') s in
                        if null s2 then 
                           error ("Unmatched { in rhythm " ++ str) else
                           faster (3/2) (rhythm1 s1) + rhythm1 (tail s2) 
    rhythm1 (c:s) = let  (dots, rst) = span (== '.') s
                         dur = fromIntegral (length dots) + 1
                         note' = slower dur note
                         more = rhythm1 rst in
      case c of 
        '*' -> note' + more
        '>' -> accent 1.3 note' + more 
        '!' -> accent 1.6 note' + more
        _ -> error ("Unexpected character " ++ [c] ++ " in rhythm " ++ str)


-- Glue multiple rhythmic objects together - check for duration errors

rhythms :: [(String, Music)] -> Music
rhythms [] = empty
rhythms (a0@((s0, m1):ns)) = if and (map (\(_,m) -> dur m == dur m1) ns) then
                                if and (map (\(s,_) -> length s == length s0) ns) then
                                   chord (map (uncurry rhythm) a0)
                                else
                                   error "Problem in rhythms: strings are not all the same length"
                              else
                                 error "Problem in rhythms: some notes are of a different duration"



-- Dynamics

-- These override underlying dynamics

ppp = E.Modify (E.Phrase [E.Dyn $ E.Accent (40/80)])
pp = E.Modify (E.Phrase [E.Dyn $ E.Accent (50/80)])
p = E.Modify (E.Phrase [E.Dyn $ E.Accent (60/80)])
mp = E.Modify (E.Phrase [E.Dyn $ E.Accent (70/80)])
fff = E.Modify (E.Phrase [E.Dyn $ E.Accent (120/80)])
ff = E.Modify (E.Phrase [E.Dyn $ E.Accent (110/80)])
f = E.Modify (E.Phrase [E.Dyn $ E.Accent (100/80)])
mf = E.Modify (E.Phrase [E.Dyn $ E.Accent (90/80)])
makeSf = E.Modify (E.Phrase [E.Dyn $ E.StdLoudness $ E.SF])

-- These change underlying dynamics

accent n = E.Modify (E.Phrase [E.Dyn $ E.Accent $ n])
ac = accent 1.5

-- Combiners

-- Use + and * for combining in sequence / parallel
instance Num (E.Music a) where
  m1 + m2 = m1 E.:+: m2
  m1 * m2 = m1 E.:=: m2
  abs n = error "No abs for music"
  signum n = error "No signum for music"
  fromInteger n = error "No signum for music"
  negate n = error "No negate for music"

line :: [Music] -> Music
line = E.line
chord :: [Music] -> Music
chord = E.chord

delay d m = slower d r + m

dur = E.dur

-- IO

save f m =
  do E.writeMidi (f ++ ".midi") m
     putStrLn ("Wrote " ++ f ++ ".midi")

-- Utilities

-- Repeat a piece of music a given number of times
rpt n m = line (take n (repeat m))

tempo :: (Music -> Music) -> Rational -> Music -> Music
tempo noteSpeed bpm mus = E.tempo (d / defaultDur) mus
  where d = bpm * dur (noteSpeed c3)
        defaultDur = 120 / 4
             
-- Pitch Collections
             
data PC = PC PCType Key [(Int, Int)] Int -- Type of collection, key, set of absolute pitch / degree, index of center.
-- Each pitch collection has a type / key that define the musical sense of the collection.  These are not
-- used by the operators that deal with fetching notes or centering but instead support pitch
-- collection transformations.

pcKey :: PC -> Key
pcKey (PC ty key pitches c) = key

pcType :: PC -> PCType
pcType (PC ty key pitches c) = ty

-- Recenter a pitch collection.

(#) :: PC -> Music -> PC
(PC ty key pitches _) # n = PC ty key pitches (length (takeWhile (\(p,d) -> (p < i)) pitches))
  where i = getPitchNote n
  
getPitchNote :: Music -> Int
getPitchNote (E.Modify _ m) = getPitchNote m
getPitchNote (E.Prim (E.Note _ d)) = E.absPitch d
getPitchNote _ = 0

  
pcCenter :: PC -> Int -> PC
pcCenter (PC ty key pitches _) p =
    PC ty key pitches (length (takeWhile (\(p',d) -> p' < p) pitches))

pcDegree :: PC -> Rational -> Int
pcDegree pc w = snd $ pcND pc w

pcPitch :: PC -> Rational -> Int
pcPitch pc w = fst $ pcND pc w

pcND :: PC -> Rational -> (Int, Int)
pcND (PC ty key pitchInfo center) w =
     if loc < 0 || loc >= length pitchInfo then (0,0)
     else pitchInfo !! loc
   where loc = (floor w) + center  -- This needs to change when pitch adjustment is added

pcNote pc w = pitchToMusic $ pcPitch pc w

pcCenterDegree pc = pcDegree pc 0
pcCenterPitch pc = pcPitch pc 0

instance Show PC where
  show pc@(PC ty k pitches c) =
       showPCType ty k ++ " starting at " ++ pitchToName (pcCenterPitch pc)
       
-- Turning a PC into music

melody :: PC -> [Rational] -> [Rational] -> Music
melody pc notes durs = addNotes notes (cycle durs)
  where
    addNotes [] ds = empty
    addNotes ns (d:ds) | d < 0 = slower (-d) r + addNotes ns ds
    addNotes (n:ns) (d:ds) =
        slower d (pcNote pc n) + addNotes ns ds
        
blockChord :: PC -> [Rational] -> Music
blockChord pc numbers =
     chord (map (\p -> pcNote pc p) numbers)
     
-- timed :: PC -> [Rational] -> [Rational] -> [Rational] -> Music

-- Descriptions of pitch collections

-- Defines the types of collections
data PCType = MajorS | MinorS | HMinorS | ChromaticS | WholeToneS | PentatonicS |
              MajorT | MinorT |
              UserDefined String [Int] deriving Show

data Key = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG |
           KeyAS | KeyBS | KeyCS | KeyDS | KeyES | KeyFS | KeyGS |
           KeyAF | KeyBF | KeyCF | KeyDF | KeyEF | KeyFF | KeyGF
        deriving Enum

keyNames = ["A", "B", "C", "D", "E", "F", "G",
            "A#", "B#", "C#", "D#", "E#", "F#", "G#",
            "AFlat", "BFlat", "CFlat", "DFlat", "EFlat", "FFlat", "GFlat"]
keySteps = [9, 11, 0, 2, 4, 5, 7,
            10, 0, 1, 3, 5, 6, 8,
            8, 10, 11, 1, 3, 4, 6]
keySigs = [KeyC, KeyCS, KeyD, KeyDS, KeyE, KeyF, KeyFS, KeyG, KeyGS, KeyA, KeyAS, KeyB]

stepsAboveC :: Key -> Int
stepsAboveC k = keySteps !! fromEnum k

transposeKey :: Key -> Int -> Key
transposeKey k n = keySigs !! (((keySteps !! fromEnum k) + n) `mod` 12)

instance Show Key where
  show k = keyNames !! (fromEnum k)

showPCType MajorS k = show k ++ " major scale"
showPCType MinorS k = show k ++ " minor scale"
showPCType MajorT k = show k ++ " major triad"
showPCType MinorT k = show k ++ " minor triad"
showPCType _ _ = "undefined"

-- scaleToPitches :: ScaleType -> [Int]
-- scaleToPitches st =
  -- case st of
     -- ChromaticS ->  repeatedScale [1]
     -- WholeToneS ->  repeatedScale [2]
     -- PentatonicS -> repeatedScale [2,2,3,2,3]   -- Not sure which one is the root!
     -- UserS p     -> p    
        
-- Predefined pitch collections

intervalsToScale :: [Int] -> Key -> [(Int, Int)]
intervalsToScale inits k = dropWhile (\(p,d) -> p < 23) $ takeWhile (\(p,d) ->  p <= maxNote) $
                           scanl (\(p', d') (delta, d) -> (p'+delta, d))
                                 (stepsAboveC k, 0)
                                 (cycle inits1)
    where inits1 = zip inits ([1..(length inits - 1)] ++ [0])     

majorScale :: Key -> PC
majorScale k = pcCenter (PC MajorS k (intervalsToScale [2,2,1,2,2,2,1] k) 0) 
                        (getPitchNote c3 + stepsAboveC k)
 
minorScale :: Key -> PC  -- Natural minor
minorScale k = pcCenter (PC MinorS k (intervalsToScale [2,1,2,2,1,2,2] k) 0) 
                        (getPitchNote c3 + stepsAboveC k)
 
majorTriad :: Key -> PC
majorTriad k = pcCenter (PC MajorT k (intervalsToScale [4,3,5] k) 0) 
                        (getPitchNote c3 + stepsAboveC k)
 
minorTriad :: Key -> PC
minorTriad k = pcCenter (PC MinorT k (intervalsToScale [3,4,5] k) 0) 
                        (getPitchNote c3 + stepsAboveC k)

maxNote :: Int
maxNote = 120


-- Transformations of chords

-- Parallel; same key but major and minor switch
tp pc =
   case pcType pc of
      MajorT -> pcCenter (minorTriad k) (if lowd == 1 then lowp-1 else lowp)
      MinorT -> pcCenter (majorTriad k) (if lowd == 1 then lowp+1 else lowp)
      _      -> error "P transform not applied to major / minor triad" 
  where lowd = pcCenterDegree pc
        lowp = pcCenterPitch pc
        k = pcKey pc

-- Relative - switch between a key and its relative major / minor
tr pc =
   case pcType pc of
      MajorT -> pcCenter (minorTriad (transposeKey k (-3))) (if lowd == 2 then lowp+2 else lowp)
      MinorT -> pcCenter (majorTriad (transposeKey k 3)) (if lowd == 0 then lowp-2 else lowp)
      _      -> error "R transform not applied to major / minor triad" 
  where lowd = pcCenterDegree pc
        lowp = pcCenterPitch pc
        k = pcKey pc
    
-- Leading tone transformation - C major to E minor 
tl pc =
   case pcType pc of
      MajorT -> pcCenter (minorTriad (transposeKey k 4)) (if lowd == 0 then lowp-1 else lowp)
      MinorT -> pcCenter (majorTriad (transposeKey k (-4))) (if lowd == 2 then lowp+1 else lowp)
      _      -> error "L transform not applied to major / minor triad" 
  where lowd = pcCenterDegree pc
        lowp = pcCenterPitch pc
        k = pcKey pc
        
keyToNote :: Key -> Music
keyToNote KeyA  = a3
keyToNote KeyAF = af3
keyToNote KeyAS = as3
keyToNote KeyB =  b3
keyToNote KeyBF = bf3
keyToNote KeyBS = bs3
keyToNote KeyC  = c3
keyToNote KeyCF = cf3
keyToNote KeyCS = cs3
keyToNote KeyD  = d3
keyToNote KeyDF = df3
keyToNote KeyDS = ds3
keyToNote KeyE  = e3
keyToNote KeyEF = ef3
keyToNote KeyES = es3
keyToNote KeyF  = f3
keyToNote KeyFF = ff3
keyToNote KeyFS = fs3
keyToNote KeyG  = g3
keyToNote KeyGF = gf3
keyToNote KeyGS = gs3  

-- Placing music at specific times

timedMusic :: [(Rational, Music)] -> Music
timedMusic l = foldr (*) empty (map (\(time, m) -> 
                                 if time == 0 then m else slower time r + m) l)

-- Random number stuff

-- This is the main thing that students will use
choices :: Int -> Int -> [a] -> [a]
choices seed n lst = take n (randomChoice (mkStdGen seed) lst)

shuffle :: Int -> [a] -> [a]
shuffle seed lst = map snd $ 
                  Data.List.sortBy (\(n1,_) (n2, _) -> compare n1 n2) $
                  zip (randomRs (0,10000::Int) (mkStdGen seed)) lst

randomNums :: Int -> Int -> [Double]
randomNums seed n = take n (randoms (mkStdGen seed))

randomChoice :: StdGen -> [a] -> [a]
randomChoice gen lst = map (lst !!) (randomRs (0, length lst-1) gen) 

transforms :: a -> [a -> a] -> [a]
transforms x [] = [x]
transforms x (f : fs) = x : transforms (f x) fs

age :: StdGen -> StdGen
age g = g' where (_, g') = next g


-- Markov Music

-- Defines a state.  The state value can be randomly varied
data State a = State (Float -> a) (Float -> (Maybe (State a)))

state :: a -> [(Float, State a)] -> State a
state value transitions =
  State (const value) (\r -> Just (getNextState r transitions))

fstate :: a -> [(Float, Maybe (State a))] -> State a
fstate value transitions =
  State (const value) (\r -> getNextState r transitions)

rstate :: (Float -> a) -> [(Float, State a)] -> State a
rstate rv transitions =
  State rv (\r -> Just (getNextState r transitions))

rfstate :: (Float -> a) -> [(Float, Maybe (State a))] -> State a
rfstate rv transitions =
  State rv (\r -> (getNextState r transitions))

getNextState :: Float -> [(Float, v)] -> v
getNextState r [(_,s)] = s
getNextState r ((p,s):t) | r <= p = s
                         | otherwise = getNextState (r-p) t

runState :: Int -> State a -> [a]
runState seed = runState1 rfloats where
  runState1 (r1:r2:rs) (State rv f) =
      let sval = rv r1 
          ns = f r2 in
        case ns of Nothing -> [sval]
                   Just v  -> sval : runState1 rs v
  rfloats :: [Float]
  rfloats = randoms (mkStdGen seed) 

newGen :: Float -> StdGen
newGen f = mkStdGen (round (1000000 * f))

chooseOne :: Float -> [a] -> a
chooseOne r l = l !! (floor (r * (fromIntegral (length l))))

-- Chaos functions
chaos :: [a] -> Int -> Int -> Int -> [a]
chaos series skip stride n = take n $ map snd $ filter (\(x,y) -> x == 0) $ 
                                            zip (cycle [0..stride-1]) (drop skip series)
											
											
tinkerbell = iterate (\(x,y) -> (x*x - y*y + 0.9*x -0.6013*y,
                                   2*x*y + 2*x + 0.5*y))
					  (-0.72, -0.64)
tinkerbell :: [(Double, Double)]


											
tent u s = iterate f s
  where f x | x < 0.5 = u*x
            | otherwise = u*(1-x)

tent1 = tent 1.5 0.61

logistic r i = iterate (\x -> r*x*(1-x)) i

logistic1 = logistic 3.6 0.4

-- RandomSelect

sel :: Double -> [a] -> a
sel n lst = lst !! (floor n `mod` (length lst))

-- Using sequences

rsequence :: (a -> Music) -> [a] -> Music
rsequence f r = line (map f r)

timedRandomly :: (a -> (Rational, Music)) -> [a] -> Music
timedRandomly f r = timedMusic (map f r)