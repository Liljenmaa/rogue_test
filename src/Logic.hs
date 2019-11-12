module Logic
(
    moveMonster,
    retrieveOutputMap,
    generateSpotMap,
    generateSpotMapFromTemplate,
    createFloorOnCoords,
    createHorFloorOnCoords,
    openDoor,
    isCmdBlockPress
) where

import Control.Applicative
import Data.Maybe

import Datatypes

dungeonMap :: DungeonMap
dungeonMap =  ["..#.....",
               "........",
               "..@.....",
               "./.+....",
               "..#.....",
               "........",
               "........",
               "........"]

-- some mon interaction here
createFloor :: Floor -> Spot -> Spot
createFloor floor spot = Spot (spotMonster spot) floor

createFloorOnCoordsInner :: CoordY -> Floor -> SpotLine -> SpotLine
createFloorOnCoordsInner y floor (s:ss)
    | y /= 0 = s : createFloorOnCoordsInner (y - 1) floor ss
    | otherwise = (createFloor floor s) : ss

createFloorOnCoords :: Coords -> Floor -> SpotMap -> SpotMap
createFloorOnCoords (x, y) floor (sl:sls)
    | x /= 0 = sl : createFloorOnCoords (x - 1, y) floor sls
    | otherwise = (createFloorOnCoordsInner y floor sl) : sls

createHorFloorOnCoordsInner :: CoordY -> Width -> Floor -> SpotLine -> SpotLine
createHorFloorOnCoordsInner y w floor (s:ss)
    | y /= 0 = s : createHorFloorOnCoordsInner (y - 1) w floor ss
    | otherwise = if w /= 0
        then (createFloor floor s) : createHorFloorOnCoordsInner 0 (w - 1) floor ss
        else s : ss

createHorFloorOnCoords :: Coords -> Width -> Floor -> SpotMap -> SpotMap
createHorFloorOnCoords (x, y) w floor (sl:sls)
    | x /= 0 = sl : createHorFloorOnCoords (x - 1, y) w floor sls
    | otherwise = (createHorFloorOnCoordsInner y w floor sl) : sls

-- createVerWall 

-- createHouse :: (CoordX, CoordY) -> (CoordX, CoordY)

setMonsterToSpotLine :: Maybe Monster -> CoordY -> SpotLine -> SpotLine
setMonsterToSpotLine mon y (s:ss)
    | y == 0 = Spot { spotMonster = mon, spotFloor = spotFloor s } : ss
    | otherwise = s : setMonsterToSpotLine mon (y - 1) ss

setMonsterToSpotMap :: Maybe Monster -> Coords -> SpotMap -> SpotMap
setMonsterToSpotMap mon (x, y) (sline:s)
    | x == 0 = setMonsterToSpotLine mon y sline : s
    | otherwise = sline : setMonsterToSpotMap mon ((x - 1), y) s

generateSpot :: Sym -> Spot
generateSpot sym = Spot Nothing (EmptyFloor sym)

generateSpotLine :: Width -> SpotLine
generateSpotLine y
    | y == 0 = []
    | otherwise = (generateSpot '.') : generateSpotLine (y - 1)

generateSpotMapInner :: Height -> Width -> SpotMap
generateSpotMapInner x y
    | x == 0 = []
    | otherwise = generateSpotLine y : generateSpotMapInner (x - 1) y 

generateSpotMap :: Height -> Width -> Coords -> SpotMap
generateSpotMap x y plCoords = setMonsterToSpotMap (Just (Player '@')) plCoords (generateSpotMapInner x y)

generateSpotFromTemplate :: Sym -> Spot
generateSpotFromTemplate sym = case sym of
    '.' -> Spot Nothing (EmptyFloor '.')
    '#' -> Spot Nothing (Wall '#')
    '+' -> Spot Nothing (Door '+' False)
    '/' -> Spot Nothing (Door '/' True)
--  '¤' -> Spot Nothing (CmdBlock something)
    '@' -> Spot (Just (Player '@')) (EmptyFloor '.')

generateSpotLineFromTemplate :: DungeonLine -> SpotLine
generateSpotLineFromTemplate dunLine = map (\x -> generateSpotFromTemplate x) dunLine

generateSpotMapFromTemplate :: DungeonMap -> SpotMap
generateSpotMapFromTemplate dmap = map (\x -> generateSpotLineFromTemplate x) dmap

retrieveSpotInner :: CoordY -> SpotLine -> Spot
retrieveSpotInner y (s:ss)
    | y /= 0 = retrieveSpotInner (y - 1) ss
    | otherwise = s

retrieveSpot :: Coords -> SpotMap -> Spot
retrieveSpot (x, y) (sl:sls)
    | x /= 0 = retrieveSpot ((x - 1), y) sls
    | otherwise = retrieveSpotInner y sl

retrieveOutputSpot :: Spot -> Sym
retrieveOutputSpot spot
    | spotMonster spot /= Nothing = symbolMon $ fromMaybe (Monster ' ') (spotMonster spot)
    | otherwise = symbolFloor $ spotFloor spot

retrieveOutputLine :: SpotLine -> OutputLine
retrieveOutputLine sline = map (\x -> retrieveOutputSpot x) sline

retrieveOutputMap :: SpotMap -> OutputMap
retrieveOutputMap smap = map (\x -> retrieveOutputLine x) smap

doActionOnCoordsInner :: CoordY -> Action -> SpotLine -> SpotLine
doActionOnCoordsInner y func (s:ss)
    | y /= 0 = s : doActionOnCoordsInner (y - 1) func ss
    | otherwise = (func s) : ss 

doActionOnCoords :: Coords -> Action -> SpotMap -> SpotMap
doActionOnCoords (x, y) func (sl:sls)
    | x /= 0 = sl : doActionOnCoords (x - 1, y) func sls
    | otherwise = (doActionOnCoordsInner y func sl) : sls

-- obsolete this
activateCmdBlock :: Coords -> SpotMap -> SpotMap
activateCmdBlock coords smap
    | isCmdBlock floor = doActionOnCoords (loc floor) (cmdAction floor) smap
    | otherwise = smap
    where floor = spotFloor $ retrieveSpot coords smap

openDoor :: Action
openDoor s
    | isClosedDoor floor = createFloor (Door '/' True) s 
    | otherwise = s
    where
        floor = spotFloor s

closeDoor :: Action
closeDoor s
    | isOpenDoor floor = createFloor (Door '+' False) s
    | otherwise = s
    where
        floor = spotFloor s

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

isCmdBlockPress :: Coords -> SpotMap -> SpotMap
isCmdBlockPress coords smap
    | isCmdBlock floor = activateCmdBlock coords smap
    | otherwise = smap
    where floor = spotFloor $ retrieveSpot coords smap

checkObstacle :: Coords -> SpotMap -> Bool
checkObstacle coords smap = isWall floor || isClosedDoor floor
    where floor = spotFloor $ retrieveSpot coords smap

checkMoveLegality :: Coords -> Direction -> SpotMap -> Bool
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

moveMonster :: Coords -> Direction -> SpotMap -> (Coords, SpotMap)
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
        monRemovedMap = setMonsterToSpotMap Nothing (x, y) smap
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
