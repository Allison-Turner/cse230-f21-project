{-# LANGUAGE FlexibleContexts #-}
module SongFile where

import Tracker.Song

import System.Directory
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC



readFromSongFile :: FilePath -> Maybe ByteString 
readFromSongFile fileName = do{
    checkFile <- doesFileExist fileName;
    if checkFile
    then Just (BC.readFile fileName)
    else Nothing
}

checkDecodeErrors :: Maybe (Song, Int) -> (Song, Int)
checkDecodeErrors Nothing = (emptySong, 0)
checkDecodeErrors (Just (song, bpm)) = (song, bpm)

deserializeSong :: FilePath -> IO (Song, Int)
deserializeSong fileName = do{
    songBytes <- readFromSongFile fileName;
    case songBytes of
        Just -> return (checkDecodeErrors (decode songBytes))
        Nothing -> return (emptySong, 0)
}

serializeSongToSongFile :: FilePath -> Song -> Int -> IO ()
serializeSongToSongFile fileName song bpm = BC.writeFile fileName (encode (goToBeginning song, bpm)) 
