module Interface
(
    mainCurses,
    goodbyeMessage
) where

import Data.Char
import UI.NCurses
import Control.Monad.IO.Class

import Game
import Cursestools
import Datatypes

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
        drawStrLn "Trying to load file \"events.txt\"..."
    render
    
    emap <- liftIO $ readEvents "events.txt"
    
    updateWindow w $ do
        drawStrLn "Loading complete. Please start the program with s."
    render
    
    waitFor w (\ev -> ev == EventCharacter 's' || ev == EventCharacter 'S')
    
    gameLoop w dmap emap
    
    clearScr w
    
    return ()

goodbyeMessage :: IO ()
goodbyeMessage = putStrLn "Thanks for playing!"
