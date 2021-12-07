module MusicFrame where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Function
import GHC.Conc (atomically, newTVar, forkIO, readTVar, threadDelay)
import Control.Monad (forever, void)

import Tracker.Song

import Menu.UI

import Brick ( Widget, hBox, simpleMain, (<=>), padAll, str, vBox, App (appStartEvent, App, appDraw, appChooseCursor, appHandleEvent, appAttrMap), neverShowCursor, BrickEvent (AppEvent), EventM, Next, attrMap, AttrMap, attrName, AttrName, fg, bg, on, withAttr, customMain, continue, halt )
import Brick.Widgets.Border(hBorder)
import qualified Brick.Widgets.Center(hCenter)
import Brick.Widgets.Center (hCenter)
import Brick.BChan (newBChan, writeBChan)
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
drawNote :: Note -> Widget ()
drawNote n =
    let totalLines = 1 + fromEnum (maxBound :: Pitch)
    in hCenter $ hBox $ map (hCenter . str) $ case n of
        Note pitch ->
            let idx = fromEnum pitch
            in replicate idx pipe ++ [show pitch] ++ replicate (totalLines - idx - 1) pipe
        Rest -> replicate totalLines pipe

-- | Draw each Note that makes up a Song in order
drawPattern :: Song -> Widget ()
drawPattern s = (vBox (map drawNote (reverse (prevNotes s))) & withAttr prevNotesAttr) <=> (drawNote (currentNote s) & withAttr currentNoteAttr) <=> (vBox (map drawNote (nextNotes s)) & withAttr nextNotesAttr)

-- | Draw a "staff" that consists of a column for each Note. 
-- | Progression upwards through the column represents the passage of Beats (musical time)
drawStaff :: Widget ()
drawStaff = 
    vBox [ hBorder 
         , hCenter $ hBox $ map (hCenter . str . show) [minBound..maxBound :: Pitch] ] & withAttr staffAttr

-- | Drawing each part of the song display 
-- | <=> puts drawPattern Widget on top of drawStaff Widget
drawSong :: Song -> [Widget Name]
drawSong song = [drawPattern song <=> drawStaff]

-- | Define how each part of the MusicFrame should look
attributeMap :: Song -> AttrMap
attributeMap _ = attrMap V.defAttr [
         (currentNoteAttr, Menu.UI.yellow `Brick.on` Menu.UI.grey `V.withStyle` V.bold)
       , (prevNotesAttr, Menu.UI.green `Brick.on` Menu.UI.grey)
       , (nextNotesAttr, Menu.UI.orange `Brick.on` Menu.UI.grey)
       , (staffAttr, fg Menu.UI.white)]

-- | TODO: handle keyboard commands for pause, exit, etc
handleEvent :: Song -> BrickEvent Name Beat -> EventM Name (Next Song)
handleEvent song (AppEvent Beat) = step song
handleEvent song e@(VtyEvent (EvKey (KChar c) [])) = continue (editSong song e) 
handleEvent song _               = continue song

-- | Advance through Song until none left, then halt
step :: Song -> EventM Name (Next Song)
step s = let s1 = forwardOneNote s in case s1 of
       Nothing -> halt s
       Just s1 -> continue s1

-- | this is where we point the UI at the Song that we want to display
-- | TODO: plug in real control structures for file system and terminal input
initSong :: IO Song
initSong = return exampleSong 

app :: App Song Beat Name
app = App
  { appDraw         = drawSong
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = attributeMap
  }

-- | This structure taken from tutorial at https://github.com/samtay/snake/blob/master/src/UI.hs
musicFrame :: IO ()
musicFrame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Beat
    threadDelay 1000000 -- decides how fast the song moves - TODO: tie this to audio bpm
  s <- initSong
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app s

editSong :: Song -> BrickEvent n e -> Song
editSong s (VtyEvent (EvKey (KChar c) [])) = Song prev (Note (toPitch c)) next
    where
        Song prev curr next = s
editSong s _ = s

toPitch :: Char -> Pitch
toPitch 'c' = C
toPitch 'C' = C
toPitch 'd' = D
toPitch 'D' = D
toPitch 'e' = E
toPitch 'E' = E
toPitch 'f' = F
toPitch 'F' = F
toPitch 'g' = G
toPitch 'G' = G
toPitch 'a' = A
toPitch 'A' = A
toPitch 'b' = B
toPitch 'B' = B
--toPitch 'c' = C'
toPitch _   = C' -- for now, in the future can possibly use capital 'C' for C and lower case 'c' for C'
