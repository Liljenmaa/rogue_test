module Game
(
    gameLoop
) where

import Datatypes
import Logic
import Cursestools
import UI.NCurses

printDungeon :: OutputMap -> Update ()
printDungeon [] = return ()
printDungeon (x:xs) = do 
    drawString x
    cursorPos <- cursorPosition
    moveCursor ((fst cursorPos) + 1) 0
    printDungeon xs

-- not ready
-- betaGameLoopInner :: Window -> SpotMap -> Curses ()
-- betaGameLoopInner w smap = do
--     
--     clearScr w
--     
--     updateWindow w $ do
--         printDungeon $ retrieveOutputMap smap
--     render
--     
--     input <- waifFor w (\ev -> True)
--     let realInput = extractEventLetter input
--     
--     if realInput == 'S' || realInput == 's'
--         then do
--             return ()
--         else do
--             let mmResult = moveMonster (x, y) realInput smap
--             let mmCoords = fst mmResult
--             let newX = fst mmCoords
--             let newY = snd mmCoords
--             let mmMap = snd mmResult
--             let newMmMap = isCmdBlockPress (newX, newY) mmMap
--             
--             betaGameLoopInner w newX newY newMmMap

gameLoopInner :: Window -> CoordX -> CoordY -> SpotMap -> Curses ()
gameLoopInner w x y smap = do
    
    clearScr w
    
    updateWindow w $ do
        printDungeon $ retrieveOutputMap smap
    render
    
    input <- waitFor w (\ev -> True)
    let realInput = extractEventLetter input
    
    if realInput == 'S' || realInput == 's'
        then do
            return ()
        else do
            let mmResult = moveMonster (x, y) realInput smap
            let mmCoords = fst mmResult
            let newX = fst mmCoords
            let newY = snd mmCoords
            let mmMap = snd mmResult
            let newMmMap = isCmdBlockPress (newX, newY) mmMap
            
            gameLoopInner w newX newY newMmMap

-- betaGameLoop :: Window -> DungeonMap -> Curses ()
-- betaGameLoop w dmap = do
--    betaGameLoopInner w $ generateSpotMapFromTemplate dmap
--    return ()

--     constructEmptySpotMapWithPlayer x y ((x `div` 2), (y `div` 2))
--     >>= createHorWall (1, 1) 2
--     >>= createDoorOnCoords (4, 4)
--     >>= createCmdBlockOnCoords (3, 1) opendoor (4, 4)
--     >>= gameLoopInner w (x `div` 2) (y `div` 2)
--     return ()

gameLoop :: Window -> Height -> Width -> Curses ()
gameLoop w x y = do
    (gl . block . door . wall) start
    return ()
    where
        start = generateSpotMap x y ((x `div` 2), (y `div` 2))
        wall = createHorFloorOnCoords (1, 1) 2 (Wall '#')
        door = createFloorOnCoords (4, 4) (Door '+' False)
        block = createFloorOnCoords (3, 1) (CmdBlock '¤' openDoor (4, 4))
        gl = gameLoopInner w (x `div` 2) (y `div` 2)
