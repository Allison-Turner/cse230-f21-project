{-# LANGUAGE BangPatterns #-}

-- | Helper function for making sounds
module Audio (initAudio, closeAudio, playNote, brieflyPlayNote, closeTheChannel) where

import Tracker.Song (Pitch(..), Note(Note, Rest))

import System.IO.Unsafe (unsafePerformIO)
import Data.Array
import Data.List
import Data.Functor
import Control.Monad
import GHC.Word
import qualified SDL.Init
import qualified SDL.Mixer as Mixer
import qualified SDL.Raw.Mixer as RawMixer
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Array (newArray)

audioConfig :: Mixer.Audio
audioConfig = Mixer.Audio
  { Mixer.audioFrequency = 22050        -- Sample rate
  , Mixer.audioFormat = Mixer.FormatU8  -- 8 bit unsigned samples
  , Mixer.audioOutput = Mixer.Stereo }

thePitches :: Array Pitch Mixer.Chunk
notPlaying :: Mixer.Chunk
(thePitches, notPlaying) =
  (array (minBound, maxBound) [(p,chunk) | p <- [minBound..maxBound], let !chunk = pitch p], zeroSound)
  where
    -- Frequency 1, RMS amplitude 1 sound
    waveform :: Double -> Double
    waveform = square
      where sawtooth x = 2 * sqrt 3 * (x - fromInteger (round x))
            sinWave x = sqrt 2 * sin (2 * pi * x)
            square x = if fromInteger (round x) > x then 1 else -1

    amplitude = 40

    -- Should probably be using queryAudio, since we're not necessarily
    -- guaranteed the settings we asked for
    sampleRate = Mixer.audioFrequency audioConfig

    semitone p = case p of
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7
      A -> 9
      B -> 11
      C' -> 12

    pitch :: Pitch -> Mixer.Chunk
    pitch p = unsafePerformIO
        $ soundDataToChunk sampleCount
        $ map f [1..sampleCount]
      where
        freq, period :: Double
        freq = (440 * 2 ** ((semitone p - semitone A) / 12)) / 2
        period = fromIntegral sampleRate / freq
        -- round 1 second to nearest number of periods
        sampleCount = round $ period * fromIntegral (round freq)

        f :: Word32 -> Word8
        f time = toEnum $ round $
          128 + amplitude * waveform (fromIntegral time / period)

    zeroSound :: Mixer.Chunk
    zeroSound = unsafePerformIO
        $ soundDataToChunk (fromIntegral sampleCount)
        $ replicate sampleCount 128
      where sampleCount = sampleRate `div` 10

    soundDataToChunk sampleCount list = do
      bytes <- newArray list
      rawChunk <- new $ RawMixer.Chunk
        { RawMixer.chunkAllocated = 0
        , RawMixer.chunkAbuf = bytes
        , RawMixer.chunkAlen = sampleCount
        , RawMixer.chunkVolume = 128
        }
      return $ Mixer.Chunk rawChunk

noteToChunk :: Note -> Mixer.Chunk
noteToChunk Rest = notPlaying
noteToChunk (Note p) = thePitches ! p

-- | A function  to be called from main
initAudio :: IO ()
initAudio = do
  SDL.Init.initialize [SDL.Init.InitAudio]
  -- from <https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer_11.html>
  let chunkSize = 1024
  Mixer.openAudio audioConfig chunkSize

-- | Another function to be called from main
closeAudio :: IO ()
closeAudio = Mixer.closeAudio


-- TODO: This will have to improve to handle polyphony
theChannel :: Mixer.Channel
theChannel = 0

playChunk :: Mixer.Chunk -> IO ()
playChunk ch = do
  oldChunk <- Mixer.playedLast theChannel
  unless (oldChunk == Just ch) $ void $ Mixer.playOn theChannel Mixer.Forever ch

playNote :: Note -> IO ()
playNote = playChunk . noteToChunk

brieflyPlayNote :: Note -> IO ()
brieflyPlayNote n = do
  Mixer.playOn theChannel Mixer.Forever (noteToChunk n)
  Mixer.fadeOut 200 theChannel

closeTheChannel :: IO ()
closeTheChannel = Mixer.pause theChannel
