module Test where

import Network
import System.IO

main = do
    handle <- connectTo "localhost" (PortNumber 4242)
    msg <- getLine
    hPutStr handle msg
    hClose handle