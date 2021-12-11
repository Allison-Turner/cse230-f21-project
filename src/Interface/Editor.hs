module Interface.Editor where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Function
import Data.List
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

toNote :: Char -> Int -> Maybe Note
toNote c oct = case elemIndex c "zsxdcvgbhnjm,l." of
  Just n -> Just (Note (Pitch (12 * oct + n)))
  Nothing -> case elemIndex c "q2w3er5t6y7ui9o0p[" of
    Just n -> Just (Note (Pitch (12 * (oct+1) + n)))
    Nothing -> if c == ' ' then Just Rest else Nothing


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
drawSong :: (Interface.Editor.Mode, Song, Int, Int) -> [Widget Name]
drawSong (_, song, bpm, oct) = [drawPattern song <=> drawStaff <=> str ("Tempo: " ++ show bpm) <=> str ("Octave: " ++ show oct)]



handleEvent :: (Interface.Editor.Mode, Song, Int, Int) -> BrickEvent Name Beat -> EventM Name (Next (Interface.Editor.Mode, Song, Int, Int))
handleEvent (_, song, b, oct) e@(VtyEvent (EvKey (KChar 'I') [])) = continue (Insert, song, b, oct)
handleEvent (_, song, b, oct) e@(VtyEvent (EvKey (KChar 'R') [])) = continue (Replace, song, b, oct)
handleEvent (_, song, b, oct) e@(VtyEvent (EvKey (KChar 'V') [])) = continue (Visual, song, b, oct)
handleEvent (m, song, b, oct) e@(VtyEvent (EvKey (KChar '+') [])) = continue (m, song, b + 4, oct)
handleEvent (m, song, b, oct) e@(VtyEvent (EvKey (KChar '-') [])) = continue (m, song, b - 4, oct)
handleEvent (m, song, b, oct) e@(VtyEvent (EvKey KRight []))      = continue (m, song, b, oct+1)
handleEvent (m, song, b, oct) e@(VtyEvent (EvKey KLeft []))       = continue (m, song, b, oct-1)
handleEvent (m, song, b, oct) e@(VtyEvent (EvKey KEsc [])) = halt (m, song, b, oct)
handleEvent (m, song, b, oct) e@(VtyEvent (EvKey _ [])) = do
  song' <- liftIO $ editSong song m oct e
  continue (m, song', b, oct)
handleEvent (m, song, b, oct) _               = continue (m, song, b, oct)



editSong :: Song -> Interface.Editor.Mode -> Int -> BrickEvent n e -> IO Song
editSong s _ _ (VtyEvent (EvKey KUp []))   = return $ fromMaybe s $ backOneNote s
editSong s _ _ (VtyEvent (EvKey KDown [])) = return $ fromMaybe s $ forwardOneNote s
editSong s _ _ (VtyEvent (EvKey KDel []))  = return $ fromMaybe s $ deleteNote s
editSong s _ _ (VtyEvent (EvKey KBS []))   = return $ fromMaybe s $ deleteNote' s
editSong s@(Song prev curr next) Insert oct (VtyEvent (EvKey (KChar c) [])) = case toNote c oct of 
    Just note -> do 
      brieflyPlayNote note
      return $ Song (note:prev) curr next 
    Nothing   -> return s
editSong s@(Song prev curr next) Replace oct (VtyEvent (EvKey (KChar c) [])) = case toNote c oct of
    Just note -> do
      brieflyPlayNote note
      let song = Song prev note next
      return $ fromMaybe song $ forwardOneNote s
    Nothing   -> return s
-- editSong s@(Song prev curr next) Visual  (VtyEvent (EvKey (KChar c) [])) = return s
editSong s _ _ _ = return s



-- | this is where we point the UI at the Song that we want to display
initSong :: Song -> Int -> Int -> IO (Interface.Editor.Mode, Song, Int, Int)
initSong s b o = return (Visual, s, b, o)

app :: App (Interface.Editor.Mode, Song, Int, Int) Beat Name
app = App
  { appDraw         = Interface.Editor.drawSong
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = \(m, s, bpm, oct) -> Interface.Editor.attributeMap (m,s)
  }



editor :: (Interface.Editor.Mode, Song, Int, Int) -> IO (Interface.Editor.Mode, Song, Int, Int)
editor = defaultMain app
