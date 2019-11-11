module Logic
(
    OutputMap (..),
    Height (..),
    Width (..),
    CoordX (..),
    CoordY (..),
    SpotMap (..),
    moveMonster,
    retrieveOutputMap,
    constructEmptySpotMapWithPlayer,
    createHorWall,
    createDoorOnCoords,
    createCmdBlockOnCoords,
    openDoor,
    isCmdBlockPress
) where

import Control.Applicative
import Data.Maybe

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

dungeonMap :: DungeonMap
dungeonMap = [".....",
               ".....",
               ".....",
               ".....",
               "....."]

createCommandBlock :: Char -> (Spot -> Spot) -> (CoordX, CoordY) -> Spot
createCommandBlock char func loc = Spot Nothing (CmdBlock char func loc)

createWall :: Char -> Spot
createWall char =  Spot Nothing (Wall char)

createDoor :: Char -> Bool -> Spot
createDoor char open =  Spot Nothing (Door char open)

generateSpot :: Char -> Spot
generateSpot char = Spot Nothing (EmptyFloor char)

generateSpotLine :: DungeonLine -> SpotLine
generateSpotLine dunLine = map (\x -> generateSpot x) dunLine

generateEmptySpotLineFromValues :: Width -> SpotLine
generateEmptySpotLineFromValues y
    | y == 0 = []
    | otherwise = (generateSpot '.') : generateEmptySpotLineFromValues (y - 1)

createHorWallInner :: CoordY -> Width -> SpotLine -> SpotLine
createHorWallInner y width (s:ss)
    | y /= 0 = s : createHorWallInner (y - 1) width ss
    | otherwise = if width /= 0 
        then (createWall '#') : createHorWallInner 0 (width - 1) ss
        else s : ss

-- might be unoptimized due to :
createHorWall :: (CoordX, CoordY) -> Width -> SpotMap -> SpotMap
createHorWall (x, y) width (sl:sls)
    | x /= 0 = sl : createHorWall ((x - 1), y) width sls
    | otherwise = (createHorWallInner y width sl) : sls

-- createVerWall 

-- createHouse :: (CoordX, CoordY) -> (CoordX, CoordY)

createCmdBlockOnCoordsInner :: CoordY -> (Spot -> Spot) -> (CoordX, CoordY) -> SpotLine -> SpotLine
createCmdBlockOnCoordsInner y func loc (s:ss)
    | y /= 0 = s : createCmdBlockOnCoordsInner (y - 1) func loc ss
    | otherwise = (createCommandBlock '¤' func loc) : ss

createCmdBlockOnCoords :: (CoordX, CoordY) -> (Spot -> Spot) -> (CoordX, CoordY) -> SpotMap -> SpotMap
createCmdBlockOnCoords (x, y) func loc (sl:sls)
    | x /= 0 = sl : createCmdBlockOnCoords (x - 1, y) func loc sls
    | otherwise = (createCmdBlockOnCoordsInner y func loc sl) : sls

createDoorOnCoordsInner :: CoordY -> SpotLine -> SpotLine
createDoorOnCoordsInner y (s:ss)
    | y /= 0 = s : createDoorOnCoordsInner (y - 1) ss
    | otherwise = (createDoor '+' False) : ss

createDoorOnCoords :: (CoordX, CoordY) -> SpotMap -> SpotMap
createDoorOnCoords (x, y) (sl:sls)
    | x /= 0 = sl : createDoorOnCoords (x - 1, y) sls
    | otherwise = (createDoorOnCoordsInner y sl) : sls 

generateSpotMap :: DungeonMap -> SpotMap
generateSpotMap dmap = map (\x -> generateSpotLine x) dmap

generateEmptySpotMapFromValues :: Height -> Width -> SpotMap
generateEmptySpotMapFromValues x y
    | x == 0 = []
    | otherwise = generateEmptySpotLineFromValues y : generateEmptySpotMapFromValues (x - 1) y

setMonsterToSpotLine :: Maybe Monster -> CoordY -> SpotLine -> SpotLine
setMonsterToSpotLine mon y (s:ss)
    | y == 0 = Spot { spotMonster = mon, spotFloor = spotFloor s } : ss
    | otherwise = s : setMonsterToSpotLine mon (y - 1) ss

setMonsterToSpotMap :: Maybe Monster -> (CoordX, CoordY) -> SpotMap -> SpotMap
setMonsterToSpotMap mon (x, y) (sline:s)
    | x == 0 = setMonsterToSpotLine mon y sline : s
    | otherwise = sline : setMonsterToSpotMap mon ((x - 1), y) s

emptyMonsterFromSpotLine :: CoordY -> SpotLine -> SpotLine
emptyMonsterFromSpotLine y (s:ss)
    | y == 0 = Spot { spotMonster = Nothing, spotFloor = spotFloor s } : ss
    | otherwise = s : emptyMonsterFromSpotLine (y - 1) ss

emptyMonsterFromSpotMap :: (CoordX, CoordY) -> SpotMap -> SpotMap
emptyMonsterFromSpotMap (x, y) (sline:s)
    | x == 0 = emptyMonsterFromSpotLine y sline : s
    | otherwise = sline : emptyMonsterFromSpotMap ((x - 1), y) s

retrieveOutputSpot :: Spot -> Sym
retrieveOutputSpot spot
    | spotMonster spot /= Nothing = symbolMon $ fromMaybe (Monster ' ') (spotMonster spot)
    | otherwise = symbolFloor $ spotFloor spot

retrieveOutputLine :: SpotLine -> OutputLine
retrieveOutputLine sline = map (\x -> retrieveOutputSpot x) sline

retrieveOutputMap :: SpotMap -> OutputMap
retrieveOutputMap smap = map (\x -> retrieveOutputLine x) smap

constructSpotMap :: SpotMap
constructSpotMap = generateSpotMap dungeonMap

constructEmptySpotMapWithPlayer :: Height -> Width -> (CoordX, CoordY) -> SpotMap
constructEmptySpotMapWithPlayer x y plCoords = setMonsterToSpotMap (Just Player { symbolMon = '@' }) plCoords (generateEmptySpotMapFromValues x y)

constructSpotMapWithPlayer :: (CoordX, CoordY) -> SpotMap
constructSpotMapWithPlayer plCoords = setMonsterToSpotMap (Just Player { symbolMon = '@' }) plCoords constructSpotMap

constructOutputMap :: OutputMap
constructOutputMap = retrieveOutputMap (constructSpotMapWithPlayer (2, 2))

retrieveSpotInner :: CoordY -> SpotLine -> Spot
retrieveSpotInner y (s:ss)
    | y /= 0 = retrieveSpotInner (y - 1) ss
    | otherwise = s

retrieveSpot :: (CoordX, CoordY) -> SpotMap -> Spot
retrieveSpot (x, y) (sl:sls)
    | x /= 0 = retrieveSpot ((x - 1), y) sls
    | otherwise = retrieveSpotInner y sl

doActionOnCoordsInner :: CoordY -> (Spot -> Spot) -> SpotLine -> SpotLine
doActionOnCoordsInner y func (s:ss)
    | y /= 0 = s : doActionOnCoordsInner (y - 1) func ss
    | otherwise = (func s) : ss 

doActionOnCoords :: (CoordX, CoordY) -> (Spot -> Spot) -> SpotMap -> SpotMap
doActionOnCoords (x, y) func (sl:sls)
    | x /= 0 = sl : doActionOnCoords (x - 1, y) func sls
    | otherwise = (doActionOnCoordsInner y func sl) : sls

activateCmdBlock :: (CoordX, CoordY) -> SpotMap -> SpotMap
activateCmdBlock coords smap
    | isCmdBlock floor = doActionOnCoords (loc floor) (cmdAction floor) smap
    | otherwise = smap
    where floor = spotFloor $ retrieveSpot coords smap

openDoor :: Spot -> Spot
openDoor s
    | isClosedDoor floor = createDoor '/' True 
    | otherwise = s
    where
        floor = spotFloor s

closeDoor :: Spot -> Spot
closeDoor s
    | isOpenDoor floor = createDoor '+' False
    | otherwise = s
    where
        floor = spotFloor s

-- make automated create function taking alter function with chassis here
openDoorInCoordsInner :: CoordY -> SpotLine -> SpotLine
openDoorInCoordsInner y (s:ss)
    | y /= 0 = s : openDoorInCoordsInner (y - 1) ss
    | otherwise = openDoor s : ss

openDoorInCoords :: (CoordX, CoordY) -> SpotMap -> SpotMap
openDoorInCoords (x, y) (sl:sls)
    | x /= 0 = sl : openDoorInCoords (x - 1, y) sls
    | otherwise = openDoorInCoordsInner y sl : sls

closeDoorInCoordsInner :: CoordY -> SpotLine -> SpotLine
closeDoorInCoordsInner y (s:ss)
    | y /= 0 = s : closeDoorInCoordsInner (y - 1) ss
    | otherwise = closeDoor s : ss

closeDoorInCoords :: (CoordX, CoordY) -> SpotMap -> SpotMap
closeDoorInCoords (x, y) (sl:sls)
    | x /= 0 = sl : closeDoorInCoords (x - 1, y) sls
    | otherwise = closeDoorInCoordsInner y sl : sls

isCmdBlock :: Floor -> Bool
isCmdBlock (CmdBlock _ _ _) = True
isCmdBlock _ = False

isWall :: Floor -> Bool
isWall (Wall _) = True
isWall _ = False

isClosedDoor :: Floor -> Bool
isClosedDoor (Door sym isOpen) = not isOpen 
isClosedDoor _ = False

isOpenDoor :: Floor -> Bool
isOpenDoor (Door sym isOpen) = isOpen
isOpenDoor _ = False

isCmdBlockPress :: (CoordX, CoordY) -> SpotMap -> SpotMap
isCmdBlockPress coords smap
    | isCmdBlock floor = activateCmdBlock coords smap
    | otherwise = smap
    where floor = spotFloor $ retrieveSpot coords smap

updateSpotMap

checkObstacle :: (CoordX, CoordY) -> SpotMap -> Bool
checkObstacle coords smap = isWall floor || isClosedDoor floor
    where floor = spotFloor $ retrieveSpot coords smap

checkMoveLegality :: (CoordX, CoordY) -> Direction -> SpotMap -> Bool
checkMoveLegality (x, y) dir smap
    | dir == 'h' = inBoundsMinY && not (checkObstacle left smap)
    | dir == 'j' = inBoundsMaxX && not (checkObstacle down smap)
    | dir == 'k' = inBoundsMinX && not (checkObstacle up smap)
    | dir == 'l' = inBoundsMaxY && not (checkObstacle right smap)
    | dir == 'y' = inBoundsMinY && inBoundsMinX && not (checkObstacle nw smap)
    | dir == 'u' = inBoundsMinX && inBoundsMaxY && not (checkObstacle ne smap)
    | dir == 'b' = inBoundsMinY && inBoundsMaxX && not (checkObstacle sw smap)
    | dir == 'n' = inBoundsMaxY && inBoundsMaxX && not (checkObstacle se smap)
    where
        inBoundsMinX = x > 0
        inBoundsMaxX = x < (length smap) - 1
        inBoundsMinY = y > 0
        inBoundsMaxY = y < (length $ smap !! 0) - 1
        up = (x - 1, y)
        down = (x + 1, y)
        left = (x, y - 1)
        right = (x, y + 1)
        nw = (x - 1, y - 1)
        ne = (x - 1, y + 1)
        sw = (x + 1, y - 1)
        se = (x + 1, y + 1)

moveMonster :: (CoordX, CoordY) -> Direction -> SpotMap -> ((CoordX, CoordY), SpotMap)
moveMonster (x, y) dir smap
    | dir == 'h' && checkMoveLegality (x, y) dir smap = (left, setMonsterToSpotMap mon left monRemovedMap)
    | dir == 'j' && checkMoveLegality (x, y) dir smap = (down, setMonsterToSpotMap mon down monRemovedMap)
    | dir == 'k' && checkMoveLegality (x, y) dir smap = (up, setMonsterToSpotMap mon up monRemovedMap)
    | dir == 'l' && checkMoveLegality (x, y) dir smap = (right, setMonsterToSpotMap mon right monRemovedMap)
    | dir == 'y' && checkMoveLegality (x, y) dir smap = (nw, setMonsterToSpotMap mon nw monRemovedMap)
    | dir == 'u' && checkMoveLegality (x, y) dir smap = (ne, setMonsterToSpotMap mon ne monRemovedMap)
    | dir == 'b' && checkMoveLegality (x, y) dir smap = (sw, setMonsterToSpotMap mon sw monRemovedMap)
    | dir == 'n' && checkMoveLegality (x, y) dir smap = (se, setMonsterToSpotMap mon se monRemovedMap)
    | otherwise = (stay, smap)
    where
        monRemovedMap = emptyMonsterFromSpotMap (x, y) smap
        mon = spotMonster $ (smap !! x) !! y
        up = (x - 1, y)
        down = (x + 1, y)
        left = (x, y - 1)
        right = (x, y + 1)
        nw = (x - 1, y - 1)
        ne = (x - 1, y + 1)
        sw = (x + 1, y - 1)
        se = (x + 1, y + 1)
        stay = (x, y)
