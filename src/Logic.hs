-- move to an EventsTxt based action performing

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
import Tools
import Actions

-- createHouse :: (CoordX, CoordY) -> (CoordX, CoordY)

-- not in use
generateSpotMap :: Height -> Width -> Coords -> SpotMap
generateSpotMap x y plCoords = doActionOnCoords plCoords (\s -> s { monSpot = Just (Player '@') }) $ inner x y
    where inner 0 y = []
          inner x y = innerInner y : inner (x - 1) y
          innerInner 0 = []
          innerInner y = innest '.' : innerInner (y - 1)
          innest sym = Spot Nothing (EmptyFloor sym)

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
generateMonCoordsFromDMap :: DungeonMap -> CoordsList
generateMonCoordsFromDMap dmap = inner (0, 0) dmap
    where inner _ [] = []
          inner (x, y) (dl:dls) = (innerInner (x, y) dl) ++ inner (x + 1, y) dls
          innerInner _ [] = []
          innerInner (x, y) (d:ds) = (innest d (x, y)) ++ innerInner (x, y + 1) ds
          innest '@' coords = [coords]
          innest  _  _      = []

makeDungeonMap :: SpotMap -> DungeonMap
makeDungeonMap smap = deepMap func smap
    where func s = case monSpot s of
                       Nothing -> symFlo $ floSpot s
                       Just x  -> symMon x

-- make it be not multiple times scrolling
setupEvents :: EventsTxt -> SpotMap -> SpotMap
setupEvents [] smap = smap
setupEvents (e:es) smap = setupEvents es $ doActionOnCoords (read (e !! 0) :: Int, read (e !! 1) :: Int) action smap
    where action = wireCmdBlock alterDoor (read (e !! 2) :: Int, read (e !! 3) :: Int)

-- is better than pattern matching version before
doActionOnCoords :: Coords -> Action -> SpotMap -> SpotMap
doActionOnCoords (x, y) func (sl:sls)
    | x == 0 = inner y func sl : sls
    | otherwise = sl : doActionOnCoords (x - 1, y) func sls
        where inner 0 func (s:ss) = func s : ss
              inner y func (s:ss) = s : inner (y - 1) func ss

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
          checkCoordsWithSameX [] = 0
          checkCoordsWithSameX (crd:crds) = inner (fst crd) 1 (crds)
              where inner _ acc [] = acc
                    inner x acc (crd:crds) = if fst crd == x
                        then inner x (acc + 1) crds
                        else acc

activateCmdBlock :: Coords -> SpotMap -> SpotMap
activateCmdBlock (x, y) smap = case floSpot $ smap !! x !! y of
    CmdBlock s c l -> doActionOnCoords l c smap
    _              -> smap

-- scrolls many times
checkAndMove :: Coords -> Coords -> SpotMap -> (Coords, SpotMap)
checkAndMove cmon@(cx, cy) check smap
    | checkInvalidMove check smap = (cmon, smap)
    | otherwise = (check, doActionOnCoords cmon removeMon $ doActionOnCoords check addMon smap)
        where checkInvalidMove (x, y) smap
                  | x < 0 || x > (length smap) - 1 || y < 0 || y > (length $ smap !! 0) - 1 = True
                  | otherwise = case floSpot $ smap !! x !! y of
                      Wall _       -> True
                      Door _ False -> True
                      _            -> False
              removeMon s = s { monSpot = Nothing }
              addMon s = s { monSpot = monSpot $ smap !! cx !! cy }

moveMonster :: Coords -> Direction -> SpotMap -> (Coords, SpotMap)
moveMonster cmon@(x, y) dir smap = checkAndMove cmon (dirToCrds dir cmon) smap
