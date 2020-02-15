module Menu (
    menuWidget,
    handleMenu,
    menuAttrMappings
) where

import Types
import Board
import IO

import Text.Read

import Lens.Micro

import qualified Graphics.Vty as V

import Brick
import qualified Brick.Widgets.Core as W

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
menuEntryLabel QuitGame = "Quit Game"

--event handling----------------------------------------------------------------

handleMenu :: BrickEvent n e -> GameState -> EventM n (Next GameState)
handleMenu (VtyEvent (V.EvKey key _)) gs = case key of
    V.KUp       -> continue $ gs & (menu . selected) %~ (\x -> max 1 (x - 1))
    V.KDown     -> continue $ gs & (menu . selected) %~ (\x -> min (length (gs ^. (menu . entries))) (x + 1))
    V.KEnter    -> case (gs ^. menu . entries) !! (gs ^. menu . selected - 1) of
                     NewGame    -> continue $ gs & dialog .~ Just newGameDialog
                     SaveGame   -> continue $ gs & dialog .~ Just saveGameDialog
                     LoadGame   -> continue $ gs & dialog .~ Just loadGameDialog
                     QuitGame   -> halt gs
    _           -> continue $ gs

--Dialogs-----------------------------------------------------------------------

newGameDialog :: Dialog
newGameDialog = Dialog "Enter dimensions" 80 "" "" "Enter an integer between 4 and 26" newGameFunc

newGameFunc :: String -> GameState -> Either (Maybe GameState) (IO GameState)
newGameFunc str gs = case readMaybe str :: Maybe Int of
    Nothing     -> Left Nothing
    Just newDim -> Left $ Just $ gs & board .~ initializeBoard newDim & focus .~ OnBoard & dialog .~ Nothing

saveGameDialog :: Dialog
saveGameDialog = Dialog "Enter path to save board state" 80 "" "" "The enterd path is not valid" saveGameFunc

saveGameFunc :: String -> GameState -> Either (Maybe GameState) (IO GameState)
saveGameFunc str gs = Right $ saveGS str gs

loadGameDialog :: Dialog
loadGameDialog = Dialog "Enter path to save board state" 80 "" "" "The enterd path is not valid" loadGameFunc

loadGameFunc :: String -> GameState -> Either (Maybe GameState) (IO GameState)
loadGameFunc str gs = Right $ loadGS str gs

--attributes--------------------------------------------------------------------


higAName :: AttrName
higAName = attrName "highlighted"

menuAttrMappings = [
    (higAName, V.green `on` V.black)
                    ]
