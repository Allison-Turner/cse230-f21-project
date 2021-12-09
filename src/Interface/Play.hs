module Interface.Play where

import Control.Monad
import GHC.Conc

import Tracker.Song
import Interface.UI
import Interface.MusicFrame

import Graphics.Vty as V

import Brick
import Brick.Types
import Brick.BChan

-- | Define how each part of the MusicFrame should look
attributeMap :: Song -> AttrMap
attributeMap _ = attrMap V.defAttr [
         (currentNoteAttr, Interface.UI.yellow `Brick.on` Interface.UI.grey `V.withStyle` V.bold)
       , (prevNotesAttr, Interface.UI.green `Brick.on` Interface.UI.grey)
       , (nextNotesAttr, Interface.UI.orange `Brick.on` Interface.UI.grey)
       , (staffAttr, fg Interface.UI.white)]



-- | Drawing each part of the song display 
-- | <=> puts drawPattern Widget on top of drawStaff Widget
drawSong :: Song -> [Widget Name]
drawSong song = [drawPattern song <=> drawStaff]



-- | TODO: handle keyboard commands for pause, exit, etc
handleEvent :: Song -> BrickEvent Name Beat -> EventM Name (Next Song)
handleEvent song (AppEvent Beat) = step song
handleEvent song _               = continue song



-- | Advance through Song until none left, then halt
step :: Song -> EventM Name (Next Song)
step s = let s1 = forwardOneNote s in case s1 of
       Nothing -> halt s
       Just s1 -> continue s1



-- | this is where we point the UI at the Song that we want to display
-- | TODO: plug in real control structures for file system and terminal input
initSong :: Song -> IO Song
initSong s = return exampleSong 



app :: App Song Beat Name
app = App
  { appDraw         = drawSong
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = attributeMap
  }



-- | This structure taken from tutorial at https://github.com/samtay/snake/blob/master/src/UI.hs
play :: Song -> IO ()
play s = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Beat
    threadDelay 1000000 -- decides how fast the song moves - TODO: tie this to audio bpm
  --s <- initSong
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app s