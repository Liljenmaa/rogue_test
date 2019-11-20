module Logic
(
    setupEvents,
    moveMonster,
    makeDungeonMap,
--  generateSpotMap,
    generateSpotMapFromTemplate,
    generateMonCoordsFromDMap,
    alterDoor,
    activateCmdBlock,
    doActionOnCoords
) where

import Control.Applicative
import Data.Maybe

import Datatypes

-- createHouse :: (CoordX, CoordY) -> (CoordX, CoordY)

createMonster :: Maybe Monster -> Spot -> Spot
createMonster mon spot = spot { spotMonster = mon }

setMonsterToSpotMap :: Maybe Monster -> Coords -> SpotMap -> SpotMap
setMonsterToSpotMap mon (0, y) (sl:sls) = inner mon y sl : sls
    where inner mon 0 (s:ss) = s { spotMonster = mon } : ss
          inner mon y (s:ss) = s : inner mon (y - 1) ss
setMonsterToSpotMap mon (x, y) (sl:sls) = sl : setMonsterToSpotMap mon ((x - 1), y) sls

deepMap :: (a -> b) -> [[a]] -> [[b]]
deepMap func mappable = map (\e -> map func e) mappable

-- scrollToInner :: Coords -> (SpotMap, SpotMap) -> (SpotMap, SpotMap)
-- scrollToInner (0, y) (prev, sl:sls) = (prev, )
-- scrollToInner (x ,y) (prev, sl:sls) = (prev : sl, scrollToInner (x - 1, y) sls)

-- scrollTo :: Coords -> SpotMap -> (SpotMap, SpotMap)
-- scrollTo coords smap = scrollToInner coords ([], smap)

-- not in use
generateSpotMap :: Height -> Width -> Coords -> SpotMap
generateSpotMap x y plCoords = setMonsterToSpotMap (Just (Player '@')) plCoords $ inner x y
    where inner 0 y = []
          inner x y = innerInner y : inner (x - 1) y
              where innerInner 0 = []
                    innerInner y = innest '.' : innerInner (y - 1)
                        where innest sym = Spot Nothing (EmptyFloor sym)

generateSpotMapFromTemplate :: DungeonMap -> SpotMap
generateSpotMapFromTemplate dmap = deepMap func dmap
    where func sym = case sym of
                         '.' -> Spot Nothing (EmptyFloor '.')
                         '#' -> Spot Nothing (Wall '#')
                         '+' -> Spot Nothing (Door '+' False)
                         '/' -> Spot Nothing (Door '/' True)
                         '¤' -> Spot Nothing (CmdBlock '¤' id (0, 0))
                         '@' -> Spot (Just (Player '@')) (EmptyFloor '.')
                         _   -> Spot Nothing (EmptyFloor '.')

-- decompose this
generateMonCoordsFromDMap :: DungeonMap -> MonCoordsList
generateMonCoordsFromDMap dmap = inner (0, 0) dmap
    where inner _ [] = []
          inner (x, y) (dl:dls) = (innerInner (x, y) dl) ++ inner (x + 1, y) dls
              where innerInner _ [] = []
                    innerInner (x, y) (d:ds) = (innest d (x, y)) ++ innerInner (x, y + 1) ds
                        where innest '@' coords = [coords]
                              innest  _  _      = []

makeDungeonMap :: SpotMap -> DungeonMap
makeDungeonMap smap = deepMap func smap
    where func s = case spotMonster s of
                       Nothing -> sym $ spotFloor s
                       Just x  -> symbolMon x

-- make it be not multiple times scrolling
setupEvents :: EventsTxt -> SpotMap -> SpotMap
setupEvents [] smap = smap
setupEvents (e:es) smap = setupEvents es $ doActionOnCoords (read (e !! 0) :: Int, read (e !! 1) :: Int) action smap
    where action = wireCmdBlock alterDoor (read (e !! 2) :: Int, read (e !! 3) :: Int)

-- looks ugly
doActionOnCoords :: Coords -> Action -> SpotMap -> SpotMap
doActionOnCoords (0, y) func (sl:sls) = inner y func sl : sls
    where inner 0 func (s:ss) = func s : ss
          inner y func (s:ss) = s : inner (y - 1) func ss
doActionOnCoords (x, y) func (sl:sls) = sl : doActionOnCoords (x - 1, y) func sls

-- fill not implemented (fills automatically)
doActionOnRectangle :: Coords -> Coords -> Action -> Bool -> SpotMap -> SpotMap
doActionOnRectangle (x, y) (z, w) func fill (sl:sls)
    | x /= 0 = sl : doActionOnRectangle (x - 1, y) (z - 1, w) func fill sls
    | z /= 0 = inner y w func fill sl : (doActionOnRectangle (0, y) (z - 1, w) func fill sls)
    | otherwise = inner y w func fill sl : sls
    where inner y w func fill (s:ss)
            | y /= 0 = s : inner (y - 1) (w - 1) func fill ss
            | w /= 0 = (func s) : inner 0 (w - 1) func fill ss
            | otherwise = (func s) : ss

-- connect this one and the one below
checkCoordsWithSameX :: [Coords] -> Int
checkCoordsWithSameX [] = 0
checkCoordsWithSameX (crd:crds) = inner (fst crd) 1 (crds)
    where inner _ acc [] = acc
          inner x acc (crd:crds) = if fst crd == x
              then inner x (acc + 1) crds
              else acc

-- Coords have to be sorted
-- Maybe you can pattern match inside a pattern match?
doActionOnMultipleCoords :: [Coords] -> Action -> SpotMap -> SpotMap
doActionOnMultipleCoords [] _ smap = smap
doActionOnMultipleCoords _ _ [] = []
doActionOnMultipleCoords coords@(crd:crds) func (sl:sls)
    | fst crd /= 0 = sl : doActionOnMultipleCoords scrollCoords func sls
    | otherwise = (inner samexYs func sl) : doActionOnMultipleCoords rest func sls
    where samexYs = map snd (take sameXInt coords)
          rest = drop sameXInt scrollCoords
          sameXInt = checkCoordsWithSameX coords
          scrollCoords = map (\(x, y) -> (x - 1, y)) coords
          inner [] _ sls = sls
          inner _ _ [] = []
          inner (0:ys) func (s:ss) = (func s) : inner (map (subtract 1) ys) func ss
          inner ys func (s:ss) = s : inner (map (subtract 1) ys) func ss

alterDoor :: Action
alterDoor s = case spotFloor s of
    Door _ False -> s { spotFloor = ((spotFloor s) { sym = '/', isOpen = True }) }
    Door _ True  -> s { spotFloor = ((spotFloor s) { sym = '+', isOpen = False }) }
    _            -> s

wireCmdBlock :: Action -> Coords -> Action
wireCmdBlock nact nloc s = case spotFloor s of
    CmdBlock _ act loc -> s { spotFloor = ((spotFloor s) { act = nact, loc = nloc }) }
    _                  -> s

activateCmdBlock :: Coords -> SpotMap -> SpotMap
activateCmdBlock (x, y) smap = case spotFloor $ smap !! x !! y of
    CmdBlock s c l -> doActionOnCoords l c smap
    _              -> smap

-- maybe make the search before input? not sure
checkObstacle :: Coords -> SpotMap -> Bool
checkObstacle (x, y) smap = case spotFloor $ smap !! x !! y of
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
    where inBoundsMinX = x > 0
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
    where monRemovedMap = setMonsterToSpotMap Nothing (x, y) smap
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
