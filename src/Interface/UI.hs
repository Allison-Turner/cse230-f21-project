module Interface.UI where

import Graphics.Vty as V

import Brick.Widgets.Core
import Brick (AttrMap, attrMap, AttrName, attrName, on)
import Brick.Widgets.FileBrowser

-- | Named resources
data Name = Name deriving (Eq, Show, Ord)

data Mode = Write | Read deriving Show
type SongFile = Maybe FilePath

data Menu = Menu{
    fileMode :: Interface.UI.Mode,
    songFile :: SongFile
} deriving Show

-- | Color macros for convenience
red, orange, yellow, green, blue, purple, pink, black, grey, white :: Color
red    = V.rgbColor 255 0 0
orange = V.rgbColor 255 128 0
yellow = V.rgbColor 255 255 0
green  = V.rgbColor 0 255 0
blue   = V.rgbColor 0 0 255
purple = V.rgbColor 128 0 255
pink   = V.rgbColor 255 0 191
black  = V.rgbColor 0 0 0
grey   = V.rgbColor 80 50 50
white  = V.rgbColor 255 255 255

-- | Usefully shaped characters
square, pipe, equals :: String 
square = "\2588"
pipe   = "|"
equals = "="

errorAttr :: AttrName
errorAttr = attrName "error"

menuAttributes :: AttrMap
menuAttributes = attrMap V.defAttr [
    (errorAttr, Interface.UI.white `on` Interface.UI.red)
    ]