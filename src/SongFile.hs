{-# LANGUAGE FlexibleContexts #-}
module SongFile where

import Tracker.Song

import System.Directory
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC



readFromSongFile :: FilePath -> IO ByteString 
readFromSongFile fileName = do{
    checkFile <- doesFileExist fileName;
    if checkFile
    then BC.readFile fileName
    else error "error in SongFile.readFromSongFile: file given by argument does not exist"
}

checkDecodeErrors :: Maybe (Song, Int) -> (Song, Int)
checkDecodeErrors Nothing = error "error in SongFile.checkDecodeErrors: argument was Nothing"
checkDecodeErrors (Just (song, bpm)) = (song, bpm)

deserializeSong :: FilePath -> IO (Song, Int)
deserializeSong fileName = do{
    songBytes <- readFromSongFile fileName;
    return (checkDecodeErrors (decode songBytes))
}

serializeSongToSongFile :: FilePath -> Song -> Int -> IO ()
serializeSongToSongFile fileName song bpm = do{
    -- checkFile <- doesFileExist fileName;
    -- if checkFile 
    -- then BC.writeFile fileName (encode song) 
    -- else error "error in SongFile.serializeSongToSongFile: checkFile was False"
    BC.writeFile fileName (encode (goToBeginning song, bpm)) 
}

makeNewSongFile = error ""