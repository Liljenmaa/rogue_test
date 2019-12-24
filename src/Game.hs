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
import Messages

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

askDirection :: Window -> MessageLog -> Coords -> String -> Curses (MessageLog, Coords)
askDirection w log crds act = do
    clearScr w
    newLog <- updateMessageLog w log ("In which direction do you want to " ++ act ++ "?")
    input <- waitFor w (\ltr -> ltrInStr ltr "hjklyubn")
    return (newLog, dirToCrds input crds)

parseInput :: Char -> (Window, Window) -> MessageLog -> Coords -> SpotMap -> Curses ()
parseInput ltr wnds log crds smap
    | ltr == 'S' || ltr == 's' = liftIO $ saveDungeon "dungeonmap.txt" $ makeDungeonMap smap
    | ltr == 'o' = (askDirection (snd wnds) log crds "open door") >>= (\(newLog, actCrds) -> gameLoopInner wnds newLog crds $ doActionOnCoords actCrds alterDoor smap)
    | ltr == 'c' = (askDirection (snd wnds) log crds "close door") >>= (\(newLog, actCrds) -> gameLoopInner wnds newLog crds $ doActionOnCoords actCrds alterDoor smap)
    | ltrInStr ltr "hjklyubn" = gameLoopInner wnds log newCrds $ activateCmdBlock newCrds mmMap
    | otherwise = gameLoopInner wnds log crds smap
        where (newCrds, mmMap) = moveMonster crds ltr smap

gameLoopInner :: (Window, Window) -> MessageLog -> Coords -> SpotMap -> Curses ()
gameLoopInner wnds log crds smap = do
    clearScr (fst wnds)
    clearScr (snd wnds)
    updateWindow (fst wnds) $ do
        printDungeon $ makeDungeonMap smap
    updateWindow (snd wnds) $ do
        printMessageLog log
    render
    input <- waitFor (fst wnds) (\ltr -> True)
    parseInput input wnds log crds smap

gameLoop :: (Window, Window) -> DungeonMap -> EventsTxt -> Curses ()
gameLoop wnds dmap emap = gameLoopInner wnds log plCoords smap
    where
        log = ["Welcome to the Game!"]
        plCoords = (generateMonCoordsFromDMap dmap) !! 0
        smap = setupEvents emap $ generateSpotMapFromTemplate dmap
