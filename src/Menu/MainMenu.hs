module MainMenu where

import MusicFrame
import Menu.UI

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

data Choice = WriteNew | EditExisting | PlayFile | Start deriving Show

drawMenu :: Choice -> [Widget ()]
drawMenu d = [C.hCenter $ padAll 1 $ (str "Terminal Tracker Menu"<=> hBorder <=> drawStartMenu)]

drawStartMenu :: Widget n
drawStartMenu = vBox [str "Start writing in a new Song file <W>",
                      str "Choose a Song file to edit <E>",
                      str "Choose a Song file to play <R>"
                     ]

appEvent :: Choice -> BrickEvent () e -> T.EventM () (T.Next Choice)
appEvent d (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc []        -> M.halt d
        V.EvKey V.KEnter []      -> M.halt d
        V.EvKey (V.KChar 'w') [] -> M.halt WriteNew
        V.EvKey (V.KChar 'e') [] -> M.halt EditExisting
        V.EvKey (V.KChar 'r') [] -> M.halt PlayFile
        _                        -> M.continue d
appEvent d _ = M.continue d


initMenu :: Choice
initMenu = Start

app :: M.App Choice e ()
app =
    M.App { M.appDraw = drawMenu
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const menuAttributes
          }

mainMenu :: IO ()
mainMenu = do
    d <- M.defaultMain MainMenu.app initMenu
    --putStrLn $ "You chose: " <> show (D.dialogSelection d)
    case d of
        WriteNew -> error "Write"
        EditExisting -> error "Edit"
        PlayFile -> musicFrame 
        _ -> error ""

