module Datatypes
(
    DungeonMap,
    Height,
    Width,
    CoordX,
    CoordY,
    Coords,
    CoordsList,
    Sym,
    SpotMap,
    Direction,
    Action,
    EventsTxt,
    MessageLog,
    Monster (..),
    Spot (..),
    Floor (..)
) where

type DungeonMap = [String]
type Height = Int
type Width = Int
type CoordX = Int
type CoordY = Int
type Coords = (CoordX, CoordY)
type CoordsList = [Coords]
type Sym = Char
type SpotMap = [[Spot]]
type Direction = Char
type Action = Spot -> Spot
type EventsTxt = [[String]]
type MessageLog = [String]

data Monster = Player {
    symMon :: Sym
} | Monster {
    symMon :: Sym
} deriving (Eq)

data Spot = Spot {
    monSpot :: Maybe Monster,
    floSpot :: Floor
}

data Floor = EmptyFloor {
    symFlo :: Sym
} | Wall {
    symFlo :: Sym
} | Door {
    symFlo :: Sym,
    isOpen :: Bool
} | CmdBlock {
    symFlo :: Sym,
    act :: Action,
    loc :: (CoordX, CoordY)
}
