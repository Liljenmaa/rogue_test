module Game
(
    gameLoop,
    loadDungeon,
    readEvents
) where

import UI.NCurses
import Control.Monad.IO.Class
import Data.List.Split

import Datatypes
import Logic
import Tools
import Cursestools

makeDungeonString :: DungeonMap -> String
makeDungeonString [] = ""
makeDungeonString (dl:dls) = dl ++ "\n" ++ makeDungeonString dls

saveDungeon :: FilePath -> DungeonMap -> IO ()
saveDungeon fp dmap = writeFile fp (makeDungeonString dmap)

loadDungeon :: FilePath -> IO (DungeonMap)
loadDungeon fp = fmap (endBy "\n") (readFile fp)

readEvents :: FilePath -> IO (EventsTxt)
readEvents fp = fmap (\x -> fmap words x) $ fmap (endBy "\n") (readFile fp)

printDungeon :: DungeonMap -> Update ()
printDungeon [] = return ()
printDungeon (x:xs) = do
    drawString x
    cursorPos <- cursorPosition
    moveCursor ((fst cursorPos) + 1) 0
    printDungeon xs

askDirection :: Window -> Coords -> String -> Curses (Coords)
askDirection w crds act = do
    updateWindow w $ do
        moveCursor 15 0
        drawString $ "In which direction do you want to " ++ act ++ "?"
    render
    input <- waitFor w (\ltr -> ltrInStr ltr "hjklyubn")
    return (dirToCrds input crds)

parseInput :: Char -> Window -> Coords -> SpotMap -> Curses ()
parseInput ltr w crds smap
    | ltr == 'S' || ltr == 's' = liftIO $ saveDungeon "dungeonmap.txt" $ makeDungeonMap smap
    | ltr == 'o' = (askDirection w crds "open door") >>= (\actCrds -> gameLoopInner w crds $ doActionOnCoords actCrds alterDoor smap)
    | ltr == 'c' = (askDirection w crds "close door") >>= (\actCrds -> gameLoopInner w crds $ doActionOnCoords actCrds alterDoor smap)
    | ltrInStr ltr "hjklyubn" = gameLoopInner w newCrds $ activateCmdBlock newCrds mmMap
    | otherwise = gameLoopInner w crds smap
        where (newCrds, mmMap) = moveMonster crds ltr smap

gameLoopInner :: Window -> Coords -> SpotMap -> Curses ()
gameLoopInner w crds smap = do
    clearScr w
    updateWindow w $ do
        printDungeon $ makeDungeonMap smap
    render
    input <- waitFor w (\ltr -> True)
    parseInput input w crds smap

gameLoop :: Window -> DungeonMap -> EventsTxt -> Curses ()
gameLoop w dmap emap = gameLoopInner w plCoords smap
    where
        plCoords = (generateMonCoordsFromDMap dmap) !! 0
        smap = setupEvents emap $ generateSpotMapFromTemplate dmap
