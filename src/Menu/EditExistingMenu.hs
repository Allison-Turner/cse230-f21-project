{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Menu.EditExistingMenu where

import Menu.UI
import Brick 
import qualified Brick.Widgets.FileBrowser as FB
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Types
import Data.Text
import Control.Exception
import qualified Graphics.Vty as V

drawFileBrowser :: (Show n, Ord n) => FB.FileBrowser n -> Widget n
drawFileBrowser b = Brick.Widgets.Center.center $ browser
    where browser = hCenter $ borderWithLabel (str "Choose a file") $ FB.renderFileBrowser True b

drawHelp b = padTop (Pad 1) $
               vBox [ case FB.fileBrowserException b of
                          Nothing -> emptyWidget
                          Just e -> hCenter $ withDefAttr errorAttr $
                                    txt $ pack $ displayException e
                    , hCenter $ str "Up/Down: select"
                    , hCenter $ str "/: search, Ctrl-C or Esc: cancel search"
                    , hCenter $ str "Enter: change directory or select file"
                    , hCenter $ str "Esc: quit"
                    ]

--initMenu :: Menu
--initMenu = Menu{
--    fileMode = Write, 
--    songFile = Nothing
--    }

drawMenu :: FB.FileBrowser n -> [Widget n]
drawMenu = error "not implemented"

initMenu = error "not implemented"


handleEvent b (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching b) -> halt b
        _ -> do
            b' <- FB.handleFileBrowserEvent ev b
            -- If the browser has a selected file after handling the
            -- event (because the user pressed Enter), shut down.
            case ev of
                V.EvKey V.KEnter [] ->
                    case FB.fileBrowserSelection b' of
                        [] -> continue b'
                        _ -> halt b'
                _ -> continue b'


app :: App (FB.FileBrowser ()) e ()
app = App { appDraw = drawMenu
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const menuAttributes
}


editExistingMenu :: IO (FB.FileBrowser ())
editExistingMenu = defaultMain app initMenu

