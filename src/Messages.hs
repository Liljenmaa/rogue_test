module Messages
(
    updateMessageLog,
    printMessageLog
) where

import UI.NCurses

import Cursestools
import Datatypes

writeMessages :: Int -> MessageLog -> Update ()
writeMessages _   []            = return ()
writeMessages 5   _             = return ()
writeMessages idx (fstmsg:rest) = drawString fstmsg >> cursorUp >> writeMessages (idx + 1) rest

-- compose these two together
updateMessageLog :: Window -> MessageLog -> String -> Curses (MessageLog)
updateMessageLog w log msg = do
    updateWindow w $ do
        moveCursor 5 0
        writeMessages 0 newLog
    render
    return newLog
        where
            newLog = msg : log

printMessageLog :: MessageLog -> Update ()
printMessageLog log = do
    moveCursor 5 0
    writeMessages 0 log
