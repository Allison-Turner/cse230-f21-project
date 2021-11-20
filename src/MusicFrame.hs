module MusicFrame where

import Data.Text

import Tracker.Song ( Pitch(C, D, E, F, G, A, B, C'), Note, Song(prevNotes, currentNote, nextNotes), exampleSong )

import Brick ( Widget, hBox, simpleMain, (<=>), padAll, str, vBox )
import Brick.Widgets.Border(hBorder)
import qualified Brick.Widgets.Center(hCenter)

import Graphics.Vty as V
import Brick.Widgets.Center (hCenter)



-- | Color macros for convenience
red = V.rgbColor 255 0 0
orange = V.rgbColor 255 128 0
yellow = V.rgbColor 255 255 0
green = V.rgbColor 0 255 0
blue = V.rgbColor 0 0 255
purple = V.rgbColor 128 0 255
pink = V.rgbColor 255 0 191
black = V.rgbColor 0 0 0
grey = V.rgbColor 80 50 50
white = V.rgbColor 255 255 255

-- | Usefully shaped characters
square = "\2588"
pipe = "|"
equals = "="



-- | Drawing each part of the song display 
-- | <=> puts drawPattern Widget on top of drawStaff Widget
drawSong :: Song -> Widget ()
drawSong song = drawPattern song <=> drawStaff



drawPattern :: Song -> Widget ()
drawPattern s = 
       let n = currentNote s
       in 
              case n of
                     C  -> hCenter $ hBox[hCenter $ str "C", hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe]
                     D  -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str "D", hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe]
                     E  -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str pipe, hCenter $ str "E", hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe]
                     F  -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str "F", hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe]
                     G  -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str "G", hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe]
                     A  -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str "A", hCenter $ str pipe, hCenter $ str pipe]
                     B  -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str "B", hCenter $ str pipe]
                     C' -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str "C'"]
                     _ -> hCenter $ hBox[hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe, hCenter $ str pipe]



drawStaff :: Widget ()
drawStaff = 
    padAll 2 $
    vBox [ hBorder 
         , hCenter $ hBox[hCenter $ str "C", 
                hCenter $ str "D",
                hCenter $ str "E",
                hCenter $ str "F",
                hCenter $ str "G",
                hCenter $ str "A",
                hCenter $ str "B",
                hCenter $ str "C'"]
         ]



musicFrame :: IO ()
musicFrame = simpleMain (drawSong exampleSong) 