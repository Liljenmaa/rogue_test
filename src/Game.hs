module Game
(
    gameLoop,
    generateDungeonMapFromFile,
    readEvents
) where

import UI.NCurses
import Control.Monad.IO.Class
import Data.List.Split

import Datatypes
import Logic
import Cursestools

makeDungeonString :: DungeonMap -> String
makeDungeonString [] = ""
makeDungeonString (dl:dls) = dl ++ "\n" ++ makeDungeonString dls

saveDungeonToFile :: FilePath -> DungeonMap -> IO ()
saveDungeonToFile fp dmap = writeFile fp (makeDungeonString dmap)

generateDungeonMapFromFile :: FilePath -> IO (DungeonMap)
generateDungeonMapFromFile fp = fmap (endBy "\n") (readFile fp)

readEvents :: FilePath -> IO (EventsTxt)
readEvents fp = fmap (\x -> fmap words x) $ fmap (endBy "\n") (readFile fp)

printDungeon :: DungeonMap -> Update ()
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
        printDungeon $ makeDungeonMap smap
    render
    
    input <- waitFor w (\ev -> True)
    let realInput = extractEventLetter input
    
    if realInput == 'S' || realInput == 's'
        then liftIO $ saveDungeonToFile "dungeonmap.txt" $ makeDungeonMap smap
        else do
            let ((newX, newY), mmMap) = moveMonster (x, y) realInput smap
            gameLoopInner w (newX, newY) $ activateCmdBlock (newX, newY) mmMap

gameLoop :: Window -> DungeonMap -> EventsTxt -> Curses ()
gameLoop w dmap emap = gameLoopInner w plCoords smap
    where
        plCoords = (generateMonCoordsFromDMap dmap) !! 0
        smap = setupEvents emap $ generateSpotMapFromTemplate dmap
