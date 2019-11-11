-- fix fromInteger bs
-- make importing logic better

module Interface
(
    mainCurses,
    goodbyeMessage
) where

import Data.Char

import Game
import Cursestools
import UI.NCurses

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
