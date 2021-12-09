module Interface.Editor where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Function
import GHC.Conc (atomically, newTVar, forkIO, readTVar, threadDelay)
import Control.Monad (forever, void)

import Tracker.Song
import Interface.UI
import Interface.MusicFrame

import Brick ( Widget, hBox, simpleMain, (<=>), padAll, str, vBox, App (appStartEvent, App, appDraw, appChooseCursor, appHandleEvent, appAttrMap), neverShowCursor, BrickEvent (AppEvent), EventM, Next, attrMap, AttrMap, attrName, AttrName, fg, bg, on, withAttr, customMain, continue, halt, defaultMain )
import Brick.Widgets.Border(hBorder)
import qualified Brick.Widgets.Center(hCenter)
import Brick.Widgets.Center (hCenter)
import Brick.BChan (newBChan, writeBChan)
import Brick.Types

import Graphics.Vty as V

data Mode = Insert | Replace | Visual deriving (Show, Eq, Ord)

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
validKey _   = False


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

-- | Define how each part of the MusicFrame should look
attributeMap :: (Interface.Editor.Mode, Song) -> AttrMap
attributeMap _ = attrMap V.defAttr [
         (currentNoteAttr, Interface.UI.yellow `Brick.on` Interface.UI.grey `V.withStyle` V.bold)
       , (prevNotesAttr, Interface.UI.green `Brick.on` Interface.UI.grey)
       , (nextNotesAttr, Interface.UI.orange `Brick.on` Interface.UI.grey)
       , (staffAttr, fg Interface.UI.white)]



-- | Drawing each part of the song display 
-- | <=> puts drawPattern Widget on top of drawStaff Widget
drawSong :: (Interface.Editor.Mode, Song) -> [Widget Name]
drawSong (_, song) = [drawPattern song <=> drawStaff]



handleEvent :: (Interface.Editor.Mode, Song) -> BrickEvent Name Beat -> EventM Name (Next (Interface.Editor.Mode, Song))
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'i') [])) = continue (Insert, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'I') [])) = continue (Insert, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'r') [])) = continue (Replace, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'R') [])) = continue (Replace, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'v') [])) = continue (Visual, song)
handleEvent (_, song) e@(VtyEvent (EvKey (KChar 'V') [])) = continue (Visual, song)
handleEvent (m, song) e@(VtyEvent (EvKey KEsc [])) = halt (m, song)
handleEvent (m, song) e@(VtyEvent (EvKey _ [])) = continue (m, (editSong song m e))
handleEvent (m, song) _               = continue (m, song)



editSong :: Song -> Interface.Editor.Mode -> BrickEvent n e -> Song
editSong s _ (VtyEvent (EvKey KUp []))   = case backOneNote s of 
    Just song -> song
    Nothing   -> s
editSong s _ (VtyEvent (EvKey KDown [])) = case forwardOneNote s of
    Just song -> song
    Nothing   -> s
editSong s _ (VtyEvent (EvKey KDel []))  = case deleteNote s of
    Just song -> song 
    Nothing   -> s
editSong s _ (VtyEvent (EvKey KBS []))   = case deleteNote' s of
    Just song -> song 
    Nothing   -> s
editSong s@(Song prev curr next) Insert  (VtyEvent (EvKey (KChar c) [])) = if validKey c then Song (curr:prev) (toNote c) next else s
editSong s@(Song prev curr next) Replace (VtyEvent (EvKey (KChar c) [])) = if validKey c then Song prev (toNote c) next else s
editSong s@(Song prev curr next) Visual  (VtyEvent (EvKey (KChar c) [])) = s
editSong s _ _ = s



-- | this is where we point the UI at the Song that we want to display
-- | TODO: plug in real control structures for file system and terminal input
initSong :: IO (Interface.Editor.Mode, Song)
initSong = return (Visual ,emptySong)

app :: App (Interface.Editor.Mode, Song) Beat Name
app = App
  { appDraw         = Interface.Editor.drawSong
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = Interface.Editor.attributeMap
  }


editor :: IO (Interface.Editor.Mode, Song)
editor = do{
    s <- initSong;
    defaultMain app s
}