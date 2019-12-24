module Cursestools
(
    receiveNumber,
    clearScr,
    cursorUp,
    cursorDown,
    drawStrLn,
    waitFor
) where

import Data.Char
import UI.NCurses

receiveNumberInner :: Window -> (Integer, Integer) -> String -> Curses (Int)
receiveNumberInner w (x, y) acc = do
    ltr' <- waitFor w (\ltr -> isDigit ltr || ltr == '\n')
    if ltr' == '\n'
        then do
            updateWindow w $ do
                cursorDown
            render
            return (read acc :: Int)
        else do
            updateWindow w $ do
                drawString [ltr']
                (_, wy) <- windowSize
                (_, cy) <- cursorPosition
                if cy == wy
                    then moveCursor x (y + 1)
                    else return ()
            render
            receiveNumberInner w (x, (y + 1)) (acc ++ [ltr'])

receiveNumber :: Window -> Curses (Int)
receiveNumber w = do
    curPos <- getCursor w
    receiveNumberInner w curPos ""

extractEventLetter :: Event -> Char
extractEventLetter (EventCharacter char) = char

clearScr :: Window -> Curses ()
clearScr w =
    updateWindow w $ do
        clear
        moveCursor 0 0

cursorUp :: Update ()
cursorUp = do
    (x, _) <- cursorPosition
    moveCursor (x - 1) 0

cursorDown :: Update ()
cursorDown = do
    (x, _) <- cursorPosition
    moveCursor (x + 1) 0

drawStrLn :: String -> Update ()
drawStrLn str = do
    drawString str
    cursorDown

waitFor :: Window -> (Char -> Bool) -> Curses (Char)
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ltr then return ltr else loop
                where ltr = extractEventLetter ev'
