module Logic
(
    moveMonster,
    retrieveOutputMap,
    makeDungeonMap,
--  generateSpotMap,
    generateSpotMapFromTemplate,
    generateMonCoordsFromDMap,
    alterDoor,
    activateCmdBlock
) where

import Control.Applicative
import Data.Maybe

import Datatypes

-- some mon interaction here
createFloor :: Floor -> Spot -> Spot
createFloor floor spot = spot { spotFloor = floor }

-- createHouse :: (CoordX, CoordY) -> (CoordX, CoordY)

createMonster :: Maybe Monster -> Spot -> Spot
createMonster mon spot = spot { spotMonster = mon }

setMonsterToSpotLine :: Maybe Monster -> CoordY -> SpotLine -> SpotLine
setMonsterToSpotLine mon 0 (s:ss) = s { spotMonster = mon } : ss
setMonsterToSpotLine mon y (s:ss) = s : setMonsterToSpotLine mon (y - 1) ss

setMonsterToSpotMap :: Maybe Monster -> Coords -> SpotMap -> SpotMap
setMonsterToSpotMap mon (0, y) (sl:sls) = setMonsterToSpotLine mon y sl : sls
setMonsterToSpotMap mon (x, y) (sl:sls) = sl : setMonsterToSpotMap mon ((x - 1), y) sls

-- not in use
generateSpot :: Sym -> Spot
generateSpot sym = Spot Nothing (EmptyFloor sym)

-- not in use
generateSpotLine :: Width -> SpotLine
generateSpotLine y
    | y == 0 = []
    | otherwise = (generateSpot '.') : generateSpotLine (y - 1)

-- not in use
generateSpotMapInner :: Height -> Width -> SpotMap
generateSpotMapInner x y
    | x == 0 = []
    | otherwise = generateSpotLine y : generateSpotMapInner (x - 1) y 

-- not in use
generateSpotMap :: Height -> Width -> Coords -> SpotMap
generateSpotMap x y plCoords = setMonsterToSpotMap (Just (Player '@')) plCoords (generateSpotMapInner x y)

-- maybe connect these two together sometime?
generateSpotFromTemplate :: Sym -> Spot
generateSpotFromTemplate sym = case sym of
    '.' -> Spot Nothing (EmptyFloor '.')
    '#' -> Spot Nothing (Wall '#')
    '+' -> Spot Nothing (Door '+' False)
    '/' -> Spot Nothing (Door '/' True)
--  '¤' -> Spot Nothing (CmdBlock something)
    '@' -> Spot (Just (Player '@')) (EmptyFloor '.')

generateSpotLineFromTemplate :: DungeonLine -> SpotLine
generateSpotLineFromTemplate dline = map generateSpotFromTemplate dline

generateSpotMapFromTemplate :: DungeonMap -> SpotMap
generateSpotMapFromTemplate dmap = map generateSpotLineFromTemplate dmap

generateMonCoordsFromSym :: Sym -> Coords -> MonCoordsList
generateMonCoordsFromSym '@' coords = [coords]
generateMonCoordsFromSym  _  _      = []

generateMonCoordsFromDLine :: Coords -> DungeonLine -> MonCoordsList
generateMonCoordsFromDLine _ [] = []
generateMonCoordsFromDLine (x, y) (d:ds) = (generateMonCoordsFromSym d (x, y)) ++ generateMonCoordsFromDLine (x, y + 1) ds

generateMonCoordsFromDMapInner :: Coords -> DungeonMap -> MonCoordsList
generateMonCoordsFromDMapInner _ [] = []
generateMonCoordsFromDMapInner (x, y) (dl:dls) = (generateMonCoordsFromDLine (x, y) dl) ++ generateMonCoordsFromDMapInner (x + 1, y) dls

generateMonCoordsFromDMap :: DungeonMap -> MonCoordsList
generateMonCoordsFromDMap dmap = generateMonCoordsFromDMapInner (0, 0) dmap

makeDungeonSpot :: Spot -> Sym
makeDungeonSpot s = case spotMonster s of
    Nothing -> symbolFloor $ spotFloor s
    Just x  -> symbolMon x

makeDungeonLine :: SpotLine -> DungeonLine
makeDungeonLine sline = map makeDungeonSpot sline

-- does not work for doors etc walkable floor that isn't empty floor
makeDungeonMap :: SpotMap -> DungeonMap
makeDungeonMap smap = map makeDungeonLine smap

retrieveSpotInner :: CoordY -> SpotLine -> Spot
retrieveSpotInner 0 (s:ss) = s
retrieveSpotInner y (s:ss) = retrieveSpotInner (y - 1) ss

retrieveSpot :: Coords -> SpotMap -> Spot
retrieveSpot (0, y) (sl:sls) = retrieveSpotInner y sl
retrieveSpot (x, y) (sl:sls) = retrieveSpot ((x - 1), y) sls

-- change name of symbolFloor to floorSym or vice versa?
retrieveOutputSpot :: Spot -> Sym
retrieveOutputSpot spot = case spotMonster spot of
    Nothing -> symbolFloor $ spotFloor spot
    Just x  -> symbolMon x

retrieveOutputLine :: SpotLine -> OutputLine
retrieveOutputLine sline = map retrieveOutputSpot sline

retrieveOutputMap :: SpotMap -> OutputMap
retrieveOutputMap smap = map retrieveOutputLine smap

doActionOnCoordsInner :: CoordY -> Action -> SpotLine -> SpotLine
doActionOnCoordsInner 0 func (s:ss) = func s : ss
doActionOnCoordsInner y func (s:ss) = s : doActionOnCoordsInner (y - 1) func ss

doActionOnCoords :: Coords -> Action -> SpotMap -> SpotMap
doActionOnCoords (0, y) func (sl:sls) = doActionOnCoordsInner y func sl : sls
doActionOnCoords (x, y) func (sl:sls) = sl : doActionOnCoords (x - 1, y) func sls

doActionOnRectangleInner :: CoordY -> CoordY -> Action -> Bool -> SpotLine -> SpotLine
doActionOnRectangleInner y w func fill (s:ss)
    | y /= 0 = s : doActionOnRectangleInner (y - 1) (w - 1) func fill ss
    | w /= 0 = (func s) : doActionOnRectangleInner 0 (w - 1) func fill ss
    | otherwise = (func s) : ss

-- fill not implemented (fills automatically)
doActionOnRectangle :: Coords -> Coords -> Action -> Bool -> SpotMap -> SpotMap
doActionOnRectangle (x, y) (z, w) func fill (sl:sls)
    | x /= 0 = sl : doActionOnRectangle (x - 1, y) (z - 1, w) func fill sls
    | z /= 0 = doActionOnRectangleInner y w func fill sl : (doActionOnRectangle (0, y) (z - 1, w) func fill sls)
    | otherwise = doActionOnRectangleInner y w func fill sl : sls

checkCoordsWithSameXInner :: CoordX -> Int -> [Coords] -> Int
checkCoordsWithSameXInner _ acc [] = acc
checkCoordsWithSameXInner x acc (crd:crds) = if fst crd == x
    then checkCoordsWithSameXInner x (acc + 1) crds
    else acc

checkCoordsWithSameX :: [Coords] -> Int
checkCoordsWithSameX [] = 0
checkCoordsWithSameX (crd:crds) = checkCoordsWithSameXInner (fst crd) 1 (crds)

doActionOnMultipleCoordsInner :: [CoordY] -> Action -> SpotLine -> SpotLine
doActionOnMultipleCoordsInner [] _ sls = sls
doActionOnMultipleCoordsInner _ _ [] = []
doActionOnMultipleCoordsInner (0:ys) func (s:ss) = (func s) : doActionOnMultipleCoordsInner (map (subtract 1) ys) func ss
doActionOnMultipleCoordsInner ys func (s:ss) = s : doActionOnMultipleCoordsInner (map (subtract 1) ys) func ss

-- Coords have to be sorted
-- Maybe you can pattern match inside a pattern match?
doActionOnMultipleCoords :: [Coords] -> Action -> SpotMap -> SpotMap
doActionOnMultipleCoords [] _ smap = smap
doActionOnMultipleCoords _ _ [] = []
doActionOnMultipleCoords coords@(crd:crds) func (sl:sls)
    | fst crd /= 0 = sl : doActionOnMultipleCoords scrollCoords func sls
    | otherwise = (doActionOnMultipleCoordsInner samexYs func sl) : doActionOnMultipleCoords rest func sls
    where
        samexYs = map snd (take sameXInt coords)
        rest = drop sameXInt scrollCoords
        sameXInt = checkCoordsWithSameX coords
        scrollCoords = map (\(x, y) -> (x - 1, y)) coords

alterDoor :: Action
alterDoor s = case spotFloor s of
    (Door _ False) -> s { spotFloor = ((spotFloor s) { isOpen = True }) }
    (Door _ True)  -> s { spotFloor = ((spotFloor s) { isOpen = False }) }
    _              -> s

activateCmdBlock :: Coords -> SpotMap -> SpotMap
activateCmdBlock coords smap = case spotFloor $ retrieveSpot coords smap of
    (CmdBlock s c l) -> doActionOnCoords l c smap
    _                -> smap

-- maybe make the search before input? not sure
checkObstacle :: Coords -> SpotMap -> Bool
checkObstacle coords smap = case spotFloor $ retrieveSpot coords smap of
    (Wall _)       -> True
    (Door _ False) -> True
    _              -> False

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


-- moveMonsterAction :: Coords -> Action -> Coords -> Action
-- moveMonsterAction remCoords addCoords

-- revamp this shit
moveMonster :: Coords -> Direction -> SpotMap -> (Coords, SpotMap)
moveMonster (x, y) dir smap
    | dir == 'h' && checkMoveLegality (x, y) dir smap = (left, doActionOnCoords left (createMonster mon) monRemovedMap)
    | dir == 'j' && checkMoveLegality (x, y) dir smap = (down, doActionOnCoords down (createMonster mon) monRemovedMap)
    | dir == 'k' && checkMoveLegality (x, y) dir smap = (up, doActionOnCoords up (createMonster mon) monRemovedMap)
    | dir == 'l' && checkMoveLegality (x, y) dir smap = (right, doActionOnCoords right (createMonster mon) monRemovedMap)
    | dir == 'y' && checkMoveLegality (x, y) dir smap = (nw, doActionOnCoords nw (createMonster mon) monRemovedMap)
    | dir == 'u' && checkMoveLegality (x, y) dir smap = (ne, doActionOnCoords ne (createMonster mon) monRemovedMap)
    | dir == 'b' && checkMoveLegality (x, y) dir smap = (sw, doActionOnCoords sw (createMonster mon) monRemovedMap)
    | dir == 'n' && checkMoveLegality (x, y) dir smap = (se, doActionOnCoords se (createMonster mon) monRemovedMap)
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
