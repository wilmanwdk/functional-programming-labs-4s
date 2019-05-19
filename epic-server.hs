module Test where

import Network
import System.IO
    
main = do
    sock <- listenOn $ PortNumber 4242
    putStrLn "Let's get the show on the road"
    handleConnections sock

handleConnections sock = do
    (handle, host, port) <- accept sock
    output <- hGetLine handle
    putStrLn output
    handleConnections sock