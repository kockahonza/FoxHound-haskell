module Menu (
    Menu,
    defMenu,
    menuWidget,
    handleMenu,
    menuAttrMappings
) where

import Types
import Board

import Text.Read

import Lens.Micro

import qualified Graphics.Vty as V

import Brick
import qualified Brick.Widgets.Core as W

--some global stuff-------------------------------------------------------------

defMenu = Menu 1 [NewGame, SaveGame, LoadGame]

--board drawing-----------------------------------------------------------------

menuWidget :: Menu -> Widget n
menuWidget mn = W.vBox finalWidgets
    where
        zippedWid = zip [1..] $ map (str . menuEntryLabel) (mn ^. entries)
        (f1, (xi, xw):f2) = break ((==(mn ^. selected)) . fst) zippedWid
        fx = (xi, withAttr higAName xw)
        finalWidgets = map snd (f1 ++ (fx:f2))

menuEntryLabel :: MenuEntry -> String
menuEntryLabel NewGame = "New Game"
menuEntryLabel SaveGame = "Save Game"
menuEntryLabel LoadGame = "Load Game"

--event handling----------------------------------------------------------------

handleMenu :: BrickEvent n e -> GameState -> GameState
handleMenu (VtyEvent (V.EvKey key _)) gs = case key of
    V.KUp       -> gs & (menu . selected) %~ (\x -> max 1 (x - 1))
    V.KDown     -> gs & (menu . selected) %~ (\x -> min (length (gs ^. (menu . entries))) (x + 1))
    V.KEnter    -> case (gs ^. menu . entries) !! (gs ^. menu . selected - 1) of
                     NewGame -> gs & dialog .~ Just newGameDialog
    _           -> gs

--Dialogs-----------------------------------------------------------------------

newGameDialog :: Dialog
newGameDialog = Dialog "Enter dimensions" 80 "" "" "Enter an integer between 4 and 26" newGameFunc

newGameFunc :: String -> GameState -> Maybe GameState
newGameFunc str gs = case readMaybe str :: Maybe Int of
    Nothing     -> Nothing
    Just newDim -> Just $ gs & board .~ initializeBoard newDim & focus .~ OnBoard & dialog .~ Nothing

--attributes--------------------------------------------------------------------


higAName :: AttrName
higAName = attrName "highlighted"

menuAttrMappings = [
    (higAName, V.green `on` V.black)
                    ]
