module Cursestools
(
    receiveNumber,
    cCheckForNumbers,
    extractEventLetter,
    clearScr,
    cursorDown,
    drawStrLn,
    waitFor
) where

import Data.Char

import Datatypes
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
