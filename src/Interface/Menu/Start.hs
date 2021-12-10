module Interface.Menu.Start where

import Interface.MusicFrame
import Interface.UI
import Interface.Play
import Interface.Editor
import Interface.Menu.ChooseSongFile



import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str, vBox, (<=>)
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T
import Brick.Widgets.Border (hBorder)
import qualified Brick.Widgets.FileBrowser as FB
import Brick.Widgets.FileBrowser (FileInfo(fileInfoFilePath))
import SongFile (deserializeSong, serializeSongToSongFile)
import Tracker.Song



data Choice = WriteNew | EditExisting | PlayFile | Quit | Start deriving Show



drawMenu :: Choice -> [Widget ()]
drawMenu d = [C.hCenter $ padAll 1 (str "Terminal Tracker Menu"<=> hBorder <=> drawStartMenu)]

drawStartMenu :: Widget n
drawStartMenu = vBox [str "Start writing in a new Song file <W>",
                      str "Choose a Song file to edit <E>",
                      str "Choose a Song file to play <R>",
                      str "Quit <Q>"
                     ]



appEvent :: Choice -> BrickEvent () e -> T.EventM () (T.Next Choice)
appEvent d (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc []        -> M.halt d
        V.EvKey V.KEnter []      -> M.halt d
        V.EvKey (V.KChar 'w') [] -> M.halt WriteNew
        V.EvKey (V.KChar 'e') [] -> M.halt EditExisting
        V.EvKey (V.KChar 'r') [] -> M.halt PlayFile
        V.EvKey (V.KChar 'q') [] -> M.halt Quit
        _                        -> M.continue d
appEvent d _ = M.continue d


initMenu :: Choice
initMenu = Start



app :: M.App Choice e ()
app =
    M.App { M.appDraw = Interface.Menu.Start.drawMenu
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const menuAttributes
          }



mainMenu :: IO ()
mainMenu = do
    d <- M.defaultMain Interface.Menu.Start.app initMenu
    case d of
        Start -> mainMenu

        WriteNew -> do{
          s <- return emptySong;
          i <- Interface.Editor.initSong s 120;
          (m,os,b) <- editor i;
          serializeSongToSongFile "./output.jsong" os b;
          mainMenu
        }

        EditExisting -> do{
          ch <- chooserApp;
          (s,b) <- deserializeSong (extractFilePath ch);
          i <- Interface.Editor.initSong s b;
          (m,os,b) <- editor i;
          serializeSongToSongFile (extractFilePath ch) os b;
          mainMenu
        }
        
        PlayFile -> do{
          ch <- chooserApp;
          (s,b) <- deserializeSong (extractFilePath ch);
          i <- Interface.Play.initSong s;
          play i b;
          mainMenu
        }

        Quit -> return ()

extractFilePath :: FB.FileBrowser n -> FilePath
extractFilePath fb = fileInfoFilePath (head (FB.fileBrowserSelection fb))

