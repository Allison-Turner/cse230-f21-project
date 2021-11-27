module Synth where

import Synthesizer.Plain.Displacement as Disp
import Sound.Sox.Play as Play
import Sound.Sox.Option.Format as SoxOpt
import Sound.Sox.Signal.List as SigS
import Synthesizer.Plain.Oscillator as Osci
import Synthesizer.Basic.Binary as BinSmp
import Synthesizer.Basic.Wave as Wave

-- import Algebra.Transcendental as Trans

import System.Exit

-- convert a midi note to a frequency, assuming equal temperament
-- i.e., a half step is a factor of 2^(1/12), and C0 = 0, C#0 = 1, ...
-- remember, A4 = 440hz; thus 69 -> 440
midiNoteToFreq :: Int -> Double
midiNoteToFreq n = 440.0 * ((2.0 Prelude.** (1.0/12.0)) Prelude.** ((fromIntegral n) - 69))

midiNoteToRatio :: Int -> Double
midiNoteToRatio n = (midiNoteToFreq n) / (11025::Double)

-- play a wave (taken from Synthesizer.Generic.Tutorial)
play :: [Double] -> IO ExitCode
play = 
    Play.simple SigS.put SoxOpt.none 11025 . -- gb has a sample rate of 11025 Hz :)
    Prelude.map BinSmp.int16FromDouble

-- polyphony demo!
demo :: IO ExitCode
demo = play $
            Osci.static Wave.square 0 (midiNoteToRatio 66)
    -- Disp.mixMulti [
    --     (Osci.static Wave.sine 0 (midiNoteToRatio 57)),
    --     (Osci.static Wave.sine 0 (midiNoteToRatio 74)),
    --     (Osci.static Wave.sine 0 (midiNoteToRatio 76)),
    --     (Osci.static Wave.sine 0 (midiNoteToRatio 79))
    -- ]

{- 
To play a rowful of notes, we just have to do something like the `demo`
function above, but taking in one note-name per line. 

However, note that the above strategy won't allow us to sustain notes -- 
if some waveform modulates over time, it'll sound funky! TODO
-}