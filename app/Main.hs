module Main where

import Servant.Server
import Network.Wai.Handler.Warp
import Todo

main :: IO ()
main = run 9090 (serve api server)