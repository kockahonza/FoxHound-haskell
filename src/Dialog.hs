module Dialog (
    drawDialog,
    handleDialog,
    dialogAttrMappings
) where

import Types

import Lens.Micro

import qualified Graphics.Vty as V

import Brick
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

drawDialog :: Dialog -> Widget n
drawDialog d = C.center $ B.border $ W.padLeftRight 10 $ W.vBox [
    str (d ^. title),
    str " ",
    withAttr inputAName $ str (d ^. input ++ replicate (d ^. inputLen - length (d ^. input)) ' '),
    str (d ^. dialogMsg)
                                                                ]

dropLastChar :: String -> String
dropLastChar str = take (length str - 1) str

handleDialog :: BrickEvent n e -> GameState -> EventM n (Next GameState)
handleDialog (VtyEvent (V.EvKey key _)) gs = case key of
    V.KEsc    -> continue $ gs & dialog .~ Nothing
    V.KChar c -> continue $ gs & (dialog . _Just . input) %~ (++[c])
    V.KBS     -> continue $ gs & (dialog . _Just . input) %~ dropLastChar
    V.KEnter  -> case (gs ^?! dialog . _Just . func) (gs ^. dialog . _Just . input) gs of
            (Left Nothing)      -> continue $ gs & (dialog . _Just . dialogMsg) .~ (gs ^. (dialog . _Just . dialogErrMsg))
            (Left (Just newGS)) -> continue newGS
            (Right gsIO)        -> suspendAndResume gsIO
    _         -> continue $ gs

inputAName :: AttrName
inputAName = attrName "input"

dialogAttrMappings = [
    (inputAName, V.black `on` V.white)
                    ]
