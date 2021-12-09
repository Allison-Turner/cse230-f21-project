{-# LANGUAGE StrictData, TemplateHaskell #-}

-- | The 'Song' data type, and helper functions for working on it
module Tracker.Song where

import Data.Aeson.TH
import GHC.Generics
import Data.Ix (Ix)

-- | A pitch
--
-- TODO: represent more pitches. For now, a single octave is fine.
data Pitch = C | D | E | F | G | A | B | C'
           deriving (Show, Eq, Ord, Enum, Bounded, Ix)

data Note = Note Pitch | Rest
          deriving (Show, Eq, Ord)

-- | A song is a sequence of notes, represented with a zipper.
data Song = Song
  { prevNotes :: [Note]
  , currentNote :: Note
  , nextNotes :: [Note]
  } deriving (Show, Eq, Ord)

forwardOneNote :: Song -> Maybe Song
forwardOneNote (Song prev curr (n:next)) = Just (Song (curr:prev) n next)
forwardOneNote (Song prev curr []) = Nothing -- End of the song!
  
backOneNote :: Song -> Maybe Song
backOneNote (Song (p:prev) curr next) = Just (Song prev p (curr:next))
backOneNote (Song [] curr next) = Nothing -- Start of the song!

deleteNote :: Song -> Maybe Song
deleteNote (Song prev _ (n:next)) = Just $ Song prev n next
deleteNote (Song (p:prev) _ [])   = Just $ Song prev p []
deleteNote (Song [] _ [])         = Nothing

deleteNote' :: Song -> Maybe Song
deleteNote' (Song (p:prev) _ next) = Just $ Song prev p next
deleteNote' (Song [] _ (n:next))   = Just $ Song [] n next
deleteNote' (Song [] _ [])         = Nothing

goToBeginning :: Song -> Song
goToBeginning (Song (p:prev) curr next) = goToBeginning (Song prev p (curr:next))
goToBeginning song = song


-- for JSON serialization and deserialization
$(deriveJSON defaultOptions ''Pitch)
$(deriveJSON defaultOptions ''Note)
$(deriveJSON defaultOptions ''Song)


exampleSong :: Song
exampleSong = Song [] curr next
  where curr:next = map Note [D,E,F,G,E,E,C,D]

emptySong :: Song
emptySong = Song [] Rest []
