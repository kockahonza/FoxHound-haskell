{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Linear.V2

import Lens.Micro
import Lens.Micro.TH (makeLenses)


--Board-------------------------------------------------------------------------

data Team = Fox | Hound deriving Eq

instance Show Team where
    show Fox = "F"
    show Hound = "H"

data Board = Board {
    _dim        :: Int,
    _cursor     :: V2 Int,
    _marked     :: Maybe (V2 Int),
    _turn       :: Team,
    _fox        :: V2 Int,
    _hounds     :: [V2 Int],
    _won        :: Maybe Team,
    _debug      :: String
                   } deriving Show

row :: Lens' (V2 Int) Int
row = lens (\(V2 r _) -> r) (\(V2 _ c) r -> V2 r c)
col :: Lens' (V2 Int) Int
col = lens (\(V2 _ c) -> c) (\(V2 r _) c -> V2 r c)

--Menu--------------------------------------------------------------------------

data MenuEntry = NewGame | SaveGame | LoadGame | QuitGame deriving Show

data Menu = Menu {
    _selected   :: Int,
    _entries    :: [MenuEntry]
                 } deriving Show

--Dialog------------------------------------------------------------------------

data Dialog = Dialog {
    _title      :: String,
    _inputLen   :: Int,
    _input      :: String,
    _message    :: String,
    _errMsg     :: String,
    _func       :: String -> GameState -> Either (Maybe GameState) (IO GameState)
                     }

instance Show Dialog where
    show (Dialog t l i m e _) = "Dialog {_title = " ++
                                show t ++
                                ", _inputLen = " ++
                                show l ++
                                ", _input = " ++
                                show i ++
                                ", _message = " ++
                                show m ++
                                ", _errMsg" ++
                                show e ++
                                ", _func = ?}"

--GameState---------------------------------------------------------------------

data Focus = OnBoard | OnMenu deriving Show

data GameState = GS {
    _focus      :: Focus,
    _board      :: Board,
    _menu       :: Menu,
    _dialog     :: Maybe Dialog
                    } deriving Show

makeLenses ''GameState
makeLenses ''Board
makeLenses ''Menu
makeLenses ''Dialog
