module Dialog where

import Types

import Data.Maybe

import Lens.Micro

import qualified Graphics.Vty as V

import Brick
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

f :: String -> GameState -> Maybe GameState
f _ gs = Nothing

defDialog = Dialog "Enter blablablah" 80 "" "" "Enter a valid number between 1 and 4 including" f

drawDialog :: Dialog -> Widget n
drawDialog d = C.center $ B.border $ W.padLeftRight 10 $ W.vBox [
    str (d ^. title),
    str " ",
    withAttr inputAName $ str (d ^. input ++ replicate (d ^. inputLen - length (d ^. input)) ' '),
    str (d ^. message)
                                                                ]

dropLastChar :: String -> String
dropLastChar str = take (length str - 1) str

handleDialog :: BrickEvent n e -> GameState -> GameState
handleDialog (VtyEvent (V.EvKey key _)) gs = case key of
    V.KChar c -> gs & (dialog . _Just . input) %~ (++[c])
    V.KBS     -> gs & (dialog . _Just . input) %~ dropLastChar
    V.KEnter  -> case (gs ^?! dialog . _Just . func) (gs ^. dialog . _Just . input) gs of
        (Just newGs)    -> newGs
        _               -> gs & (dialog . _Just . message) .~ (gs ^. (dialog . _Just . errMsg))
    _         -> gs

inputAName :: AttrName
inputAName = attrName "input"

dialogAttrMappings = [
    (inputAName, V.black `on` V.white)
                    ]
