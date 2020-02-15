module Board (
    initializeBoard,
    boardWidget,
    handleBoard,
    boardAttrMappings,
    checkWin
) where

import           Types

import           Data.List
import           Text.Printf

import           Linear.V2

import           Lens.Micro

import qualified Graphics.Vty       as V

import           Brick
import qualified Brick.Widgets.Core as W

--some global stuff-------------------------------------------------------------

foxAllowedMoves = [V2 r c | r <- [-1, 1], c <- [-1, 1]]
houndsAllowedMoves = [V2 1 c | c <- [-1, 1]]

initializeBoard :: Int -> Board
initializeBoard dim = Board dim (V2 1 1) Nothing Fox fox hounds Nothing ""
    where
        fox = V2 dim (if even dim then 1 + 2 * ((dim + 1) `div` 4) else 2 + 2 * (dim `div` 4))
        hounds = map (V2 1) [2,4..dim]

--board drawing-----------------------------------------------------------------

boardWidget :: Board -> Widget n
boardWidget bs = W.vBox $ infoWidget : intersperse vDelimitWidget lineWidgets ++ [str (bs ^. boardMsg)]
    where
        d = bs ^. dim
        dDigits = 1 + floor (logBase 10 (fromIntegral d :: Float)) :: Int
        vLegWForStr = "%0" ++ show dDigits ++ "d"
        prefixSpacing = replicate (dDigits + 1) ' '
        infoWidget = case bs ^. won of
                        (Just Fox)      -> withAttr wonAName $ str "Fox won"
                        (Just Hound)    -> withAttr wonAName $ str "Hounds won"
                        Nothing         -> case bs ^. turn of
                                            Fox   -> str "Fox to move"
                                            Hound -> str "Hounds to move"
        vDelimitWidget = str $ prefixSpacing ++ concat (replicate d "|===") ++ "|"
        hLegendWidget = str (prefixSpacing ++ "  " ++ intercalate "   " (map (:[]) (take d ['A'..'Z'])))
        lineWidgets = hLegendWidget : rowWidgets ++ [hLegendWidget]
        rowWidgets = [rowWidget r | r <- [1..d]]
        rowWidget r = W.hBox $ intersperse hDelimitWidget boxWidgets
            where
                hDelimitWidget = str " | "
                vLegendWidget = str (printf vLegWForStr r)
                boxWidgets = vLegendWidget : posWidgets ++ [vLegendWidget]
                posWidgets = [posToWidget (V2 r c) | c <- [1..d]]
                posToWidget p
                  | p == bs ^. cursor = withAttr curAName $ str $ posToLabel p
                  | Just p == bs ^. marked = withAttr selAName $ str $ posToLabel p
                  | otherwise = str $ posToLabel p
                    where
                        posToLabel o
                          | o == bs ^. fox = "F"
                          | o `elem` bs ^. hounds = "H"
                          | otherwise = " "

--event handling----------------------------------------------------------------

handleBoard :: BrickEvent n e -> Board -> Board
handleBoard (VtyEvent (V.EvKey key _)) bs = case key of
    V.KUp       -> moveFieldBy row (-1)
    V.KDown     -> moveFieldBy row 1
    V.KRight    -> moveFieldBy col 1
    V.KLeft     -> moveFieldBy col (-1)
    V.KChar ' ' -> case bs ^. marked of
                    Nothing -> bs & marked ?~ (bs ^. cursor)
                    _       -> checkWin $ tryMove bs
    _           -> bs
    where
        moveFieldBy field by = bs & cursor . field %~ updateCoord by
        updateCoord delta origin = if (dest >= 1) && (dest <= (bs ^. dim)) then dest else origin
            where
                dest = origin + delta
handleBoard _ bs = bs

tryMove :: Board -> Board
tryMove bs@(Board dim des (Just ori) Fox f hs w m)
  | ori /= f || des `elem` hs || (des - ori) `notElem` foxAllowedMoves = bs & marked .~ Nothing
  | otherwise = Board dim des Nothing Hound des hs w m
tryMove bs@(Board dim des (Just ori) Hound f hs w m)
  | ori `notElem` hs || des `elem` (f:hs) || (des - ori) `notElem` houndsAllowedMoves = bs & marked .~ Nothing
  | otherwise = Board dim des Nothing Fox f newHs w m
    where
        newHs = des : filter (/=ori) hs

foxAllValidMoves :: Board -> [V2 Int]
foxAllValidMoves bs = filter (all (<= d)) $ filter (all (>= 1)) $ filter (`notElem` hs) $ map (+ f) foxAllowedMoves
    where
        d = bs ^. dim
        f = bs ^. fox
        hs = bs ^. hounds

houndAllValidMoves :: Board -> V2 Int -> [V2 Int]
houndAllValidMoves bs h = filter (all (<= d)) $ filter (all (>= 1)) $ filter (`notElem` (f:filter (/= h) hs)) $ map (+ h) houndsAllowedMoves
    where
        d = bs ^. dim
        f = bs ^. fox
        hs = bs ^. hounds

checkWin :: Board -> Board
checkWin bs
  | bs ^. (fox . row) == 1 || all null (map (houndAllValidMoves bs) (bs ^. hounds)) = bs & won ?~ Fox
  | null (foxAllValidMoves bs) = bs & won ?~ Hound
  | otherwise = bs & won .~ Nothing

--attributes--------------------------------------------------------------------

curAName :: AttrName
curAName = attrName "cursor"

selAName :: AttrName
selAName = attrName "marked"

wonAName :: AttrName
wonAName = attrName "won"

boardAttrMappings = [
    (curAName, V.black `on` V.yellow),
    (selAName, V.black `on` V.red),
    (wonAName, V.cyan `on` V.yellow)
                    ]
