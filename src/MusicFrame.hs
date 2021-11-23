module MusicFrame where

import Data.Text (Text)
import qualified Data.Text as T

import Tracker.Song

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
    let totalLines = 1 + fromEnum (maxBound :: Pitch)
    in hCenter $ hBox $ map (hCenter . str) $ case currentNote s of
        Note pitch ->
            let idx = fromEnum pitch
            in replicate idx pipe ++ [show pitch] ++ replicate (totalLines - idx - 1) pipe
        Rest -> replicate totalLines pipe


drawStaff :: Widget ()
drawStaff = 
    padAll 2 $
    vBox [ hBorder 
         , hCenter $ hBox $ map (hCenter . str . show) [minBound..maxBound :: Pitch] ]


musicFrame :: IO ()
musicFrame = simpleMain (drawSong exampleSong) 
