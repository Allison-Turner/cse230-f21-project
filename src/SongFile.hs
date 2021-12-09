{-# LANGUAGE FlexibleContexts #-}
module SongFile where

import System.Directory

import Tracker.Song
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

checkDecodeErrors :: Maybe Song -> Song
checkDecodeErrors Nothing = error "error in SongFile.checkDecodeErrors: argument was Nothing"
checkDecodeErrors (Just song) = song

deserializeSong :: FilePath -> IO Song
deserializeSong fileName = do{
    songBytes <- readFromSongFile fileName;
    return (checkDecodeErrors (decode songBytes))
}

serializeSongToSongFile :: FilePath -> Song -> IO ()
serializeSongToSongFile fileName song = do{
    checkFile <- doesFileExist fileName;
    if checkFile 
    then BC.writeFile fileName (encode song) 
    else error "error in SongFile.serializeSongToSongFile: checkFile was False"
}

makeNewSongFile = error ""