module Main where

import Types
import Board
import Menu
import Dialog

import Data.Maybe

import Lens.Micro

import qualified Graphics.Vty as V

import Brick
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

--some global values------------------------------------------------------------

defBoard = initializeBoard 8
defMenu = Menu 1 [NewGame, SaveGame, LoadGame, QuitGame]
defGS = GS OnBoard defBoard defMenu Nothing

--------------------------------------------------------------------------------

mainApp :: (Ord n) => App GameState e n
mainApp = App {
    appDraw = drawMain,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleMain,
    appStartEvent = return,
    appAttrMap = attrMapMain
              }

drawMain :: GameState -> [Widget n]
drawMain (GS _ _ _ (Just d)) = [drawDialog d]
drawMain gs = [C.center $ W.hBox $ case gs ^. focus of
                OnBoard -> [withBorderStyle BS.unicodeBold $ borderedBoard, borderedMenu]
                OnMenu  -> [borderedBoard, withBorderStyle BS.unicodeBold $ borderedMenu]
              ]
    where
        borderedBoard = B.border $ W.padLeftRight 3 $ W.padTopBottom 1 $ boardWidget (gs ^. board)
        borderedMenu  = B.border $ W.padLeftRight 5 $ W.padTopBottom 5 $ menuWidget (gs ^. menu)

handleMain :: GameState -> BrickEvent n e -> EventM n (Next GameState)
handleMain gs@(GS _ _ _ (Just d)) e = handleDialog e gs
handleMain (GS OnBoard b m d) (VtyEvent (V.EvKey (V.KChar '\t') _)) = continue (GS OnMenu b m d)
handleMain (GS OnMenu b m d)  (VtyEvent (V.EvKey (V.KChar '\t') _)) = continue (GS OnBoard b m d)
handleMain gs e = case gs ^. focus of
                  OnBoard   -> continue $ gs & board %~ handleBoard e
                  OnMenu    -> handleMenu e gs

attrMapMain :: GameState -> AttrMap
attrMapMain = const (attrMap V.defAttr (boardAttrMappings ++ menuAttrMappings ++ dialogAttrMappings))

main :: IO ()
main = do
    defaultMain (mainApp :: App GameState e ()) defGS
    print "Thanks for playing"
