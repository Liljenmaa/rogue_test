module Interface
(
    mainCurses,
    goodbyeMessage
) where

import Data.Char
import UI.NCurses
import Control.Monad.IO.Class

import Game
import Tools
import Cursestools
import Datatypes

mainCurses :: IO ()
mainCurses = runCurses $ do
    setEcho False
    defW <- defaultWindow
    logW <- cloneWindow defW
    
    defWSize <- updateWindow defW windowSize
    defWCoords <- updateWindow defW windowPosition
    
    updateWindow logW $ do
        resizeWindow 6 (snd defWSize)
        moveWindow 15 (snd defWCoords)
    
    clearScr defW
    clearScr logW
    
    updateWindow defW $ do
        drawStrLn "Welcome to the alpha version of [REDACTED]!"
        drawStrLn "Trying to load file \"dungeonmap.txt\"..."
    render
    
    dmap <- liftIO $ loadDungeon "dungeonmap.txt"
    
    updateWindow defW $ do
        drawStrLn "Trying to load file \"events.txt\"..."
    render
    
    emap <- liftIO $ readEvents "events.txt"
    
    updateWindow defW $ do
        drawStrLn "Loading complete. Please start the program with s."
    render
    
    waitFor defW (\ltr -> ltr == 's' || ltr == 'S')
    
    let wnds = (defW, logW)
    gameLoop wnds dmap emap
    
    clearScr defW
    
    return ()

goodbyeMessage :: IO ()
goodbyeMessage = putStrLn "Thanks for playing!"
