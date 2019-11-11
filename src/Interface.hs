-- fix fromInteger bs
-- make importing logic better

module Interface
(
    mainCurses,
    goodbyeMessage
) where

import Data.Char
import Logic
import UI.NCurses

receiveNumberInner :: Window -> Integer -> Integer -> String -> Curses (Int)
receiveNumberInner w x y acc = do
    ev' <- waitFor w (\ev -> cCheckForNumbers ev || ev == EventCharacter '\n')
    if ev' == EventCharacter '\n'
        then do
            updateWindow w $ do
                cursorDown
            render
            return (read acc :: Int)
        else do
            updateWindow w $ do
                drawString [extractEventLetter ev']
                winSize <- windowSize
                curPos <- cursorPosition
                if (snd curPos) == (snd winSize)
                    then moveCursor x ( y + 1)
                    else return ()
            render
            receiveNumberInner w x (y + 1) (acc ++ [extractEventLetter ev'])

receiveNumber :: Window -> Curses (Int)
receiveNumber w = do
    curPos <- getCursor w
    receiveNumberInner w (fst curPos) (snd curPos) ""

cCheckForNumbers :: Event -> Bool
cCheckForNumbers (EventCharacter num) = isDigit num

extractEventLetter :: Event -> Char
extractEventLetter (EventCharacter char) = char

clearScr :: Window -> Curses ()
clearScr w =
    updateWindow w $ do
        clear
        moveCursor 0 0

cursorDown :: Update ()
cursorDown = do
    cursorPos <- cursorPosition
    moveCursor (fst cursorPos + 1) 0

drawStrLn :: String -> Update ()
drawStrLn str = do
    drawString str
    cursorDown

waitFor :: Window -> (Event -> Bool) -> Curses (Event)
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return (ev') else loop

printDungeon :: OutputMap -> Update ()
printDungeon [] = return ()
printDungeon (x:xs) = do 
    drawString x
    cursorPos <- cursorPosition
    moveCursor ((fst cursorPos) + 1) 0
    printDungeon xs

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
        start = constructEmptySpotMapWithPlayer x y ((x `div` 2), (y `div` 2))
        wall = createHorWall (1, 1) 2
        door = createDoorOnCoords (4, 4)
        block = createCmdBlockOnCoords (3, 1) openDoor (4, 4)
        gl = gameLoopInner w (x `div` 2) (y `div` 2)

mainCurses :: IO ()
mainCurses = runCurses $ do
    setEcho False
    w <- defaultWindow
    
    clearScr w
    
    updateWindow w $ do
        drawStrLn "Welcome to the alpha version of [REDACTED]!"
        drawStrLn "Let's setup some variables first."
        drawStrLn "What should the map x be?"
    render
    
    mapX <- receiveNumber w
    
    updateWindow w $ do
        drawStrLn "How about the map y?"
    render
    
    mapY <- receiveNumber w
    
    updateWindow w $ do
        drawStrLn $ show mapX ++ " " ++ show mapY
        drawStrLn "Thanks. Please start the program with s"
    render
    
    waitFor w (\ev -> ev == EventCharacter 's' || ev == EventCharacter 'S')
    
    gameLoop w mapX mapY
    
    clearScr w
    
    return ()

goodbyeMessage :: IO ()
goodbyeMessage = putStrLn "Thanks for playing!"
