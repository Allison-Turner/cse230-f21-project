module Interface.Play where

import Control.Monad
import Control.Monad.IO.Class
import GHC.Conc

import Tracker.Song
import Interface.UI
import Interface.MusicFrame

import Audio

import Graphics.Vty as V

import Brick
    ( (<=>),
      Widget,
      AttrMap,
      attrMap,
      continue,
      customMain,
      halt,
      neverShowCursor,
      fg,
      on,
      str,
      App(..),
      EventM,
      BrickEvent(AppEvent),
      Next )
import Brick.Types
import Brick.BChan

-- | Define how each part of the MusicFrame should look
attributeMap :: (Song, PlayMode) -> AttrMap
attributeMap _ = attrMap V.defAttr [
         (currentNoteAttr, Interface.UI.white `Brick.on` bkgColor `V.withStyle` V.bold)
       , (prevNotesAttr, Interface.UI.darkgrey `Brick.on` Interface.UI.black)
       , (nextNotesAttr, Interface.UI.grey `Brick.on` Interface.UI.black)
       , (staffAttr, fg Interface.UI.white)]

bkgColor :: Color
bkgColor = Interface.UI.grey

-- | Drawing each part of the song display 
-- | <=> puts drawPattern Widget on top of drawStaff Widget
drawSong :: (Song, PlayMode) -> [Widget Name]
drawSong (song, _) = [drawPattern song <=> drawStaff <=> str "Space: pause/continue the song\nEsc: quit player"]

data PlayMode = Pause | Resume deriving (Show, Eq)

-- | TODO: handle keyboard commands for pause, exit, etc
handleEvent :: (Song, PlayMode) -> BrickEvent Name Beat -> EventM Name (Next (Song, PlayMode))
handleEvent (song, m) (AppEvent Beat) = step (song, m)
handleEvent (song, _) (VtyEvent (EvKey KEsc [])) = do 
  liftIO (closeTheChannel)
  halt (song, Resume)
handleEvent (song, Pause) (VtyEvent (EvKey (KChar ' ') []))  = continue (song, Resume)
handleEvent (song, Resume) (VtyEvent (EvKey (KChar ' ') [])) = continue (song, Pause)
handleEvent (song, m) _               = continue (song, m)



-- | Advance through Song until none left, then halt
step :: (Song, PlayMode) -> EventM Name (Next (Song, PlayMode))
step (s, Pause)  = do
  liftIO (closeTheChannel)
  continue (s, Pause)
step (s, Resume) = let s1 = forwardOneNote s in case s1 of
       Nothing -> do
         liftIO (closeTheChannel)
         halt (s, Resume)
       Just s1 -> do 
         liftIO (playNote (currentNote s1))
         continue (s1, Resume)



-- | this is where we point the UI at the Song that we want to display
-- | TODO: plug in real control structures for file system and terminal input
initSong :: Song -> IO Song
initSong = return



app :: App (Song, PlayMode) Beat Name
app = App
  { appDraw         = drawSong
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = attributeMap
  }



-- | This structure taken from tutorial at https://github.com/samtay/snake/blob/master/src/UI.hs
play :: Song -> Int -> IO ()
play s bpm = do
  playNote (currentNote s)  -- funky off-by-one error otherwise
  chan <- newBChan 10
  forkIO $ forever $ do
    threadDelay (bpmToMicrosecondDelay bpm)
    writeBChan chan Beat
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app (s, Resume)


bpmToMicrosecondDelay :: Int -> Int
bpmToMicrosecondDelay = (60000000 `div`)