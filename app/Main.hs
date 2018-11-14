module Main where

import Servant.Server
import Network.Wai.Handler.Warp
import qualified Todo
import TodoWeb
import Control.Monad.State

app :: Todo.Todos -> Application
app s = serve api $ hoistServer api (flip evalStateT s) server

main :: IO ()
main = run 9090 $ app Todo.createNew
