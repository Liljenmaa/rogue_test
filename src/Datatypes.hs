module Datatypes
(
    DungeonLine,
    DungeonMap,
    OutputLine,
    OutputMap,
    Height,
    Width,
    CoordX,
    CoordY,
    Sym,
    SpotLine,
    SpotMap,
    Direction,
    Monster (..),
    Spot (..),
    Floor (..)
) where

type DungeonLine = String
type DungeonMap = [String]
type OutputLine = String
type OutputMap = [String]
type Height = Int
type Width = Int
type CoordX = Int
type CoordY = Int
type Sym = Char
type SpotLine = [Spot]
type SpotMap = [SpotLine]
type Direction = Char

data Monster = Player {
    symbolMon :: Sym
} | Monster {
    symbolMon :: Sym
} deriving (Eq)

data Spot = Spot {
    spotMonster :: Maybe Monster,
    spotFloor :: Floor
}

data Floor = EmptyFloor {
    symbolFloor :: Sym
} | Wall {
    symbolFloor :: Sym
} | Door {
    symbolFloor :: Sym,
    isOpen :: Bool
} | CmdBlock {
    symbolFloor :: Sym,
    cmdAction :: (Spot -> Spot),
    loc :: (CoordX, CoordY)
}
