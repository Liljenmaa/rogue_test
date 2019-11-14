module Logic
(
    moveMonster,
    retrieveOutputMap,
    makeDungeonMap,
--  generateSpotMap,
    generateSpotMapFromTemplate,
    generateMonCoordsFromDMap,
    openDoor,
    isCmdBlockPress
) where

import Control.Applicative
import Data.Maybe

import Datatypes

-- some mon interaction here
createFloor :: Floor -> Spot -> Spot
createFloor floor spot = Spot (spotMonster spot) floor

-- createHouse :: (CoordX, CoordY) -> (CoordX, CoordY)

createMonster :: Maybe Monster -> Spot -> Spot
createMonster mon spot = Spot mon (spotFloor spot)

setMonsterToSpotLine :: Maybe Monster -> CoordY -> SpotLine -> SpotLine
setMonsterToSpotLine mon y (s:ss)
    | y == 0 = Spot { spotMonster = mon, spotFloor = spotFloor s } : ss
    | otherwise = s : setMonsterToSpotLine mon (y - 1) ss

setMonsterToSpotMap :: Maybe Monster -> Coords -> SpotMap -> SpotMap
setMonsterToSpotMap mon (x, y) (sline:s)
    | x == 0 = setMonsterToSpotLine mon y sline : s
    | otherwise = sline : setMonsterToSpotMap mon ((x - 1), y) s

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

generateMonCoordsFromSym :: Sym -> Coords -> MonCoordsList
generateMonCoordsFromSym sym coords = case sym of
    '@' -> [coords];
     _  -> []

generateMonCoordsFromDLine :: Coords -> DungeonLine -> MonCoordsList
generateMonCoordsFromDLine _ [] = []
generateMonCoordsFromDLine (x, y) (d:ds) = (generateMonCoordsFromSym d (x, y)) ++ generateMonCoordsFromDLine (x, y + 1) ds

generateMonCoordsFromDMapInner :: Coords -> DungeonMap -> MonCoordsList
generateMonCoordsFromDMapInner _ [] = []
generateMonCoordsFromDMapInner (x, y) (dl:dls) = (generateMonCoordsFromDLine (x, y) dl) ++ generateMonCoordsFromDMapInner (x + 1, y) dls

generateMonCoordsFromDMap :: DungeonMap -> MonCoordsList
generateMonCoordsFromDMap dmap = generateMonCoordsFromDMapInner (0, 0) dmap

generateSpotLineFromTemplate :: DungeonLine -> SpotLine
generateSpotLineFromTemplate dunLine = map generateSpotFromTemplate dunLine

generateSpotMapFromTemplate :: DungeonMap -> SpotMap
generateSpotMapFromTemplate dmap = map generateSpotLineFromTemplate dmap

makeDungeonSpot :: Spot -> Sym
makeDungeonSpot s
    | spotMonster s /= Nothing = symbolMon $ fromMaybe (Dummy) $ spotMonster s
    | otherwise = symbolFloor $ spotFloor s

makeDungeonLine :: SpotLine -> DungeonLine
makeDungeonLine sline = map makeDungeonSpot sline

-- does not work for doors etc walkable floor that isn't empty floor
makeDungeonMap :: SpotMap -> DungeonMap
makeDungeonMap smap = map makeDungeonLine smap

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
checkCoordsWithSameX (crd:crds) = checkCoordsWithSameXInner (fst crd) 1 (crds)

doActionOnMultipleCoordsInner :: [CoordY] -> Action -> SpotLine -> SpotLine
doActionOnMultipleCoordsInner [] _ sls = sls
doActionOnMultipleCoordsInner _ _ [] = []
doActionOnMultipleCoordsInner (y:ys) func (s:ss)
    | y /= 0 = s : doActionOnMultipleCoordsInner (map (subtract 1) (y:ys)) func ss
    | otherwise = (func s) : doActionOnMultipleCoordsInner (map (subtract 1) ys) func ss

-- Coords have to be sorted
doActionOnMultipleCoords :: [Coords] -> Action -> SpotMap -> SpotMap
doActionOnMultipleCoords [] _ smap = smap
doActionOnMultipleCoords _ _ [] = []
doActionOnMultipleCoords (crd:crds) func (sl:sls)
    | fst crd /= 0 = sl : doActionOnMultipleCoords scrollCoords func sls
    | otherwise = (doActionOnMultipleCoordsInner samexYs func sl) : doActionOnMultipleCoords rest func sls
    where
        samexYs = map (\x -> snd x) (take sameXInt (crd:crds))
        rest = drop sameXInt scrollCoords
        sameXInt = checkCoordsWithSameX (crd:crds)
        scrollCoords = map (\(x, y) -> (x - 1, y)) (crd:crds)

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
