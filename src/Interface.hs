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

mainCurses :: IO ()
mainCurses = runCurses $ do
    setEcho False
    w <- defaultWindow
    
    clearScr w
    
    updateWindow w $ do
        drawStrLn "Welcome to the alpha version of [REDACTED]!"
        drawStrLn "Trying to load file \"dungeonmap.txt\"..."
--      drawStrLn "Let's setup some variables first."
--      drawStrLn "What should the map x be?"
    render
    
--  mapX <- receiveNumber w
    
--  updateWindow w $ do
--      drawStrLn "How about the map y?"
--  render
    
--  mapY <- receiveNumber w
    
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
