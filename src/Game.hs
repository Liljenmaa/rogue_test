module Game
(
    gameLoop
) where

import UI.NCurses

import Datatypes
import Logic
import Cursestools

printDungeon :: OutputMap -> Update ()
printDungeon [] = return ()
printDungeon (x:xs) = do 
    drawString x
    cursorPos <- cursorPosition
    moveCursor ((fst cursorPos) + 1) 0
    printDungeon xs

gameLoopInner :: Window -> Coords -> SpotMap -> Curses ()
gameLoopInner w (x, y) smap = do
    
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
            
            gameLoopInner w (newX, newY) newMmMap

-- get rid of do?
gameLoop :: Window -> DungeonMap -> Curses ()
gameLoop w dmap = do
    gameLoopInner w plCoords $ generateSpotMapFromTemplate dmap
    return ()
    where
        plCoords = (generateMonCoordsFromDMap dmap) !! 0
