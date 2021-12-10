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
      App(..),
      EventM,
      BrickEvent(AppEvent),
      Next )
import Brick.Types
import Brick.BChan

-- | Define how each part of the MusicFrame should look
attributeMap :: Song -> AttrMap
attributeMap _ = attrMap V.defAttr [
         (currentNoteAttr, Interface.UI.white `Brick.on` (bkgColor) `V.withStyle` V.bold)
       , (prevNotesAttr, Interface.UI.darkgrey `Brick.on` Interface.UI.black)
       , (nextNotesAttr, Interface.UI.grey `Brick.on` Interface.UI.black)
       , (staffAttr, fg Interface.UI.white)]

bkgColor :: Color
bkgColor = Interface.UI.grey

-- | Drawing each part of the song display 
-- | <=> puts drawPattern Widget on top of drawStaff Widget
drawSong :: Song -> [Widget Name]
drawSong song = [drawPattern song <=> drawStaff]



-- | TODO: handle keyboard commands for pause, exit, etc
handleEvent :: Song -> BrickEvent Name Beat -> EventM Name (Next Song)
handleEvent song (AppEvent Beat) = step song
handleEvent song (VtyEvent (EvKey KEsc [])) = do 
  liftIO (closeTheChannel)
  halt song
handleEvent song (VtyEvent (EvKey KEsc [])) = halt song
handleEvent song _               = continue song



-- | Advance through Song until none left, then halt
step :: Song -> EventM Name (Next Song)
step s = let s1 = forwardOneNote s in case s1 of
       Nothing -> do
         liftIO (closeTheChannel)
         halt s
       Just s1 -> do 
         liftIO (playNote (currentNote s1))
         continue s1



-- | this is where we point the UI at the Song that we want to display
-- | TODO: plug in real control structures for file system and terminal input
initSong :: Song -> IO Song
initSong s = return s 



app :: App Song Beat Name
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
    threadDelay (bpmToMicrosecondDelay bpm) -- decides how fast the song moves - TODO: tie this to audio bpm
    writeBChan chan Beat
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app s


bpmToMicrosecondDelay :: Int -> Int
bpmToMicrosecondDelay = (60000000 `div`)