module Main where

import Interface.Menu.Start
import Audio

main :: IO ()
main = do
    initAudio
    mainMenu
    closeAudio
