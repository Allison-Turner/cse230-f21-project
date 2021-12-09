module Interface.MusicFrame where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Function

import Tracker.Song

import Interface.UI

import Brick ( Widget, hBox, simpleMain, (<=>), padAll, str, vBox, App (appStartEvent, App, appDraw, appChooseCursor, appHandleEvent, appAttrMap), neverShowCursor, BrickEvent (AppEvent), EventM, Next, attrMap, AttrMap, attrName, AttrName, fg, bg, on, withAttr, customMain, continue, halt )
import Brick.Widgets.Border(hBorder)
import qualified Brick.Widgets.Center(hCenter)
import Brick.Widgets.Center (hCenter)
import Brick.Types

import Graphics.Vty as V



-- | Data type to drive the passage of time in units of a musical "beat"
data Beat = Beat



-- | Define any attributes we might apply for styling
-- | sort of like adding a CSS class to an HTML element, so any CSS rules for that class are applied to it. this is the list of CSS classes
currentNoteAttr, prevNotesAttr, nextNotesAttr, pitchAttr, restAttr, staffAttr :: AttrName
currentNoteAttr = attrName "currentNoteAttr"
prevNotesAttr   = attrName "prevNotesAttr"
nextNotesAttr   = attrName "nextNotesAttr"
pitchAttr       = attrName "pitchAttr"
restAttr        = attrName "restAttr"
staffAttr       = attrName "staffAttr"



-- | Draw a single Note, aligned with the "staff" at the bottom of the screen
drawNote :: Note -> Widget n
drawNote n =
    let totalLines = 1 + fromEnum (maxBound :: Pitch)
    in hCenter $ hBox $ map (hCenter . str) $ case n of
        Note pitch ->
            let idx = fromEnum pitch
            in replicate idx pipe ++ [show pitch] ++ replicate (totalLines - idx - 1) pipe
        Rest -> replicate totalLines pipe

-- | Draw each Note that makes up a Song in order
drawPattern :: Song -> Widget n
drawPattern s = vBox (map (withAttr prevNotesAttr . drawNote) (reverse (prevNotes s))) <=> 
                vBox [ (withAttr currentNoteAttr . drawNote) (currentNote s) ] <=> 
                vBox (map (withAttr nextNotesAttr . drawNote) (nextNotes s))

-- | Draw a "staff" that consists of a column for each Note. 
-- | Progression upwards through the column represents the passage of Beats (musical time)
drawStaff :: Widget n
drawStaff = 
    vBox [ hBorder 
         , hCenter $ hBox $ map (hCenter . str . show) [minBound..maxBound :: Pitch] ] & withAttr staffAttr

