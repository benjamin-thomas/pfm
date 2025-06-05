module Main where

import Server qualified
import Server2 qualified

main :: IO ()
main =
    if False
        then
            Server2.start 8080
        else
            Server.runServer 8080
