module Editor where

--this is where James will build his editor brick app

-- James' Editor should have an App{} object passed to a customMain or defaultMain
-- an event handler for each type of keyboard control
-- etc.

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

data Mode = Insert | Overwrite deriving (Show, Eq, Ord)

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
drawSong :: (Editor.Mode, Song) -> [Widget Name]
drawSong (_, song) = [drawPattern song <=> drawStaff]

-- | Define how each part of the MusicFrame should look
attributeMap :: (Editor.Mode, Song) -> AttrMap
attributeMap _ = attrMap V.defAttr [
         (currentNoteAttr, Editor.yellow `Brick.on` Editor.grey `V.withStyle` V.bold)
       , (prevNotesAttr, Editor.green `Brick.on` Editor.grey)
       , (nextNotesAttr, Editor.orange `Brick.on` Editor.grey)
       , (staffAttr, fg Editor.white)]

-- | TODO: handle keyboard commands for pause, exit, etc
handleEvent :: (Editor.Mode, Song) -> BrickEvent Name Beat -> EventM Name (Next (Editor.Mode, Song))
-- handleEvent song (AppEvent Beat) = step song
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'i') [])) = continue (Insert, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'I') [])) = continue (Insert, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'o') [])) = continue (Overwrite, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'O') [])) = continue (Overwrite, song)
handleEvent (m, song) e@(VtyEvent (EvKey KEsc [])) = halt (m, song)
handleEvent (m, song) e@(VtyEvent (EvKey _ [])) = continue (m, (editSong song m e))
handleEvent (m, song) _               = continue (m, song)

-- | Advance through Song until none left, then halt
step :: Song -> EventM Name (Next Song)
step s = let s1 = forwardOneNote s in case s1 of
       Nothing -> halt s
       Just s1 -> continue s1

-- | this is where we point the UI at the Song that we want to display
-- | TODO: plug in real control structures for file system and terminal input
initSong :: IO (Editor.Mode, Song)
initSong = return (Insert ,emptySong)

app :: App (Editor.Mode, Song) Beat Name
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

editSong :: Song -> Editor.Mode -> BrickEvent n e -> Song
editSong s _ (VtyEvent (EvKey KUp []))   = case backOneNote s of 
    Just song -> song
    Nothing   -> s
editSong s _ (VtyEvent (EvKey KDown [])) = case forwardOneNote s of
    Just song -> song
    Nothing   -> s
editSong s _ (VtyEvent (EvKey KDel []))  = case deleteNote s of
    Just song -> song 
    Nothing   -> s
editSong s@(Song prev curr next) Insert    (VtyEvent (EvKey (KChar c) [])) = if validKey c then Song (prev ++ [curr]) (toNote c) next else s
editSong s@(Song prev curr next) Overwrite (VtyEvent (EvKey (KChar c) [])) = if validKey c then Song prev (toNote c) next else s
editSong s _ _ = s

validKey :: Char -> Bool 
validKey 'c' = True
validKey 'C' = True
validKey 'd' = True
validKey 'D' = True
validKey 'e' = True
validKey 'E' = True
validKey 'f' = True
validKey 'F' = True
validKey 'g' = True
validKey 'G' = True
validKey 'a' = True
validKey 'A' = True
validKey 'b' = True
validKey 'B' = True
validKey 's' = True
validKey 'S' = True
validKey ' ' = True


toNote :: Char -> Note
toNote 'c' = Note C
toNote 'C' = Note C
toNote 'd' = Note D
toNote 'D' = Note D
toNote 'e' = Note E
toNote 'E' = Note E
toNote 'f' = Note F
toNote 'F' = Note F
toNote 'g' = Note G
toNote 'G' = Note G
toNote 'a' = Note A
toNote 'A' = Note A
toNote 'b' = Note B
toNote 'B' = Note B
toNote 's' = Note C' 
toNote 'S' = Note C'
toNote ' ' = Rest
toNote _   = Rest