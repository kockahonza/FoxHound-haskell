{-# LANGUAGE TemplateHaskell           #-}

module Types (
    Team(..),
    Board(..),
    dim,
    cursor,
    marked,
    turn,
    fox,
    hounds,
    won,
    boardMsg,
    row,
    col,
    MenuEntry(..),
    Menu(..),
    selected,
    entries,
    Dialog(..),
    title,
    inputLen,
    input,
    dialogMsg,
    dialogErrMsg,
    func,
    Focus(..),
    GameState(..),
    focus,
    board,
    menu,
    dialog
) where

import           Linear.V2

import           Lens.Micro
import           Lens.Micro.TH (makeLenses)


--Board-------------------------------------------------------------------------

data Team = Fox
    | Hound
    deriving (Eq, Show, Read)

data Board = Board
    { _dim      :: Int
    , _cursor   :: V2 Int
    , _marked   :: Maybe (V2 Int)
    , _turn     :: Team
    , _fox      :: V2 Int
    , _hounds   :: [V2 Int]
    , _won      :: Maybe Team
    , _boardMsg :: String
    }
    deriving Show

instance Eq Board where
    (Board d _ _ t f hs _ _) == (Board d' _ _ t' f' hs' _ _) = d == d
                                                            && t == t
                                                            && f == f
                                                            && hs == hs

row :: Lens' (V2 Int) Int
row = lens (\(V2 r _) -> r) (\(V2 _ c) r -> V2 r c)
col :: Lens' (V2 Int) Int
col = lens (\(V2 _ c) -> c) (\(V2 r _) c -> V2 r c)

--Menu--------------------------------------------------------------------------

data MenuEntry = NewGame
    | SaveGame
    | LoadGame
    | QuitGame
    deriving Show

data Menu = Menu
    { _selected :: Int
    , _entries  :: [MenuEntry]
    }
    deriving Show

--Dialog------------------------------------------------------------------------

data Dialog = Dialog
    { _title :: String
    , _inputLen :: Int
    , _input :: String
    , _dialogMsg :: String
    , _dialogErrMsg :: String
    , _func :: String -> GameState -> Either (Maybe GameState) (IO GameState)
    }

--GameState---------------------------------------------------------------------

data Focus = OnBoard
    | OnMenu
    deriving Show

data GameState = GS
    { _focus  :: Focus
    , _board  :: Board
    , _menu   :: Menu
    , _dialog :: Maybe Dialog
    }

makeLenses ''GameState
makeLenses ''Board
makeLenses ''Menu
makeLenses ''Dialog
