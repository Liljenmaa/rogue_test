module Interface
(
    mainCurses,
    goodbyeMessage
) where

import Data.Char
import Data.List.Split
import UI.NCurses
import Control.Monad.IO.Class

import Game
import Cursestools
import Datatypes

generateDungeonMapFromFile :: FilePath -> IO (DungeonMap)
generateDungeonMapFromFile fp = fmap (endBy "\n") (readFile fp)

mainCurses :: IO ()
mainCurses = runCurses $ do
    setEcho False
    w <- defaultWindow
    
    clearScr w
    
    updateWindow w $ do
        drawStrLn "Welcome to the alpha version of [REDACTED]!"
        drawStrLn "Trying to load file \"dungeonmap.txt\"..."
    render
    
    dmap <- liftIO $ generateDungeonMapFromFile "dungeonmap.txt"
    
    updateWindow w $ do
        drawStrLn "File loaded. Please start the program with s."
    render
    
    waitFor w (\ev -> ev == EventCharacter 's' || ev == EventCharacter 'S')
    
    gameLoop w dmap
    
    clearScr w
    
    return ()

goodbyeMessage :: IO ()
goodbyeMessage = putStrLn "Thanks for playing!"
