module Interface.Editor where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Function
import GHC.Conc (atomically, newTVar, forkIO, readTVar, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Maybe

import Tracker.Song
import Interface.UI
import Interface.MusicFrame
import Audio

import Brick ( Widget, hBox, simpleMain, (<=>), padAll, str, vBox, App (appStartEvent, App, appDraw, appChooseCursor, appHandleEvent, appAttrMap), neverShowCursor, BrickEvent (AppEvent), EventM, Next, attrMap, AttrMap, attrName, AttrName, fg, bg, on, withAttr, customMain, continue, halt, defaultMain )
import Brick.Widgets.Border(hBorder)
import qualified Brick.Widgets.Center(hCenter)
import Brick.Widgets.Center (hCenter)
import Brick.BChan (newBChan, writeBChan)
import Brick.Types

import Graphics.Vty as V

data Mode = Insert | Replace | Visual deriving (Show, Eq, Ord)

toNote :: Char -> Maybe Note
toNote 'q' = Just (Note 60)
toNote 'w' = Just (Note 62)
toNote 'e' = Just (Note 64)
toNote 'r' = Just (Note 65)
toNote 't' = Just (Note 67)
toNote 'y' = Just (Note 69)
toNote 'u' = Just (Note 71)
toNote 'i' = Just (Note 72) 
toNote ' ' = Just (Rest)
toNote _   = Nothing

-- | Define how each part of the MusicFrame should look
attributeMap :: (Interface.Editor.Mode, Song) -> AttrMap
attributeMap (m, _) = attrMap V.defAttr [
         (currentNoteAttr, Interface.UI.white `Brick.on` (bkgColor m) `V.withStyle` V.bold)
       , (prevNotesAttr, Interface.UI.grey `Brick.on` Interface.UI.black)
       , (nextNotesAttr, Interface.UI.lightgrey `Brick.on` Interface.UI.black)
       , (staffAttr, fg Interface.UI.white)]

bkgColor :: Interface.Editor.Mode -> Color
bkgColor Insert = Interface.UI.darkred
bkgColor Replace = Interface.UI.darkblue
bkgColor Visual = Interface.UI.grey

-- | Drawing each part of the song display 
-- | <=> puts drawPattern Widget on top of drawStaff Widget
drawSong :: (Interface.Editor.Mode, Song, Int) -> [Widget Name]
drawSong (_, song, bpm) = [drawPattern song <=> drawStaff <=> str ("Tempo: " ++ show bpm)]



handleEvent :: (Interface.Editor.Mode, Song, Int) -> BrickEvent Name Beat -> EventM Name (Next (Interface.Editor.Mode, Song, Int))
handleEvent (_, song, b) e@(VtyEvent (EvKey (KChar 'I') [])) = continue (Insert, song, b)
handleEvent (_, song, b) e@(VtyEvent (EvKey (KChar 'R') [])) = continue (Replace, song, b)
handleEvent (_, song, b) e@(VtyEvent (EvKey (KChar 'V') [])) = continue (Visual, song, b)
handleEvent (m, song, b) e@(VtyEvent (EvKey (KChar '+') [])) = continue (m, song, b + 4)
handleEvent (m, song, b) e@(VtyEvent (EvKey (KChar '-') [])) = continue (m, song, b - 4)
handleEvent (m, song, b) e@(VtyEvent (EvKey KEsc [])) = halt (m, song, b)
handleEvent (m, song, b) e@(VtyEvent (EvKey _ [])) = do
  song' <- liftIO $ editSong song m e
  continue (m, song', b)
handleEvent (m, song, b) _               = continue (m, song, b)



editSong :: Song -> Interface.Editor.Mode -> BrickEvent n e -> IO Song
editSong s _ (VtyEvent (EvKey KUp []))   = return $ fromMaybe s $ backOneNote s
editSong s _ (VtyEvent (EvKey KDown [])) = return $ fromMaybe s $ forwardOneNote s
editSong s _ (VtyEvent (EvKey KDel []))  = return $ fromMaybe s $ deleteNote s
editSong s _ (VtyEvent (EvKey KBS []))   = return $ fromMaybe s $ deleteNote' s
editSong s@(Song prev curr next) Insert  (VtyEvent (EvKey (KChar c) [])) = case toNote c of 
    Just note -> do 
      brieflyPlayNote note
      return $ Song (note:prev) curr next 
    Nothing   -> return s
editSong s@(Song prev curr next) Replace (VtyEvent (EvKey (KChar c) [])) = case toNote c of
    Just note -> do
      brieflyPlayNote note
      let song = Song prev note next
      return $ fromMaybe song $ forwardOneNote s
    Nothing   -> return s
-- editSong s@(Song prev curr next) Visual  (VtyEvent (EvKey (KChar c) [])) = return s
editSong s _ _ = return s



-- | this is where we point the UI at the Song that we want to display
initSong :: Song -> Int -> IO (Interface.Editor.Mode, Song, Int)
initSong s b = return (Visual, s, b)

app :: App (Interface.Editor.Mode, Song, Int) Beat Name
app = App
  { appDraw         = Interface.Editor.drawSong
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = \(m, s, bpm) -> Interface.Editor.attributeMap (m,s)
  }



editor :: (Interface.Editor.Mode, Song, Int) -> IO (Interface.Editor.Mode, Song, Int)
editor = defaultMain app
