module Main where

import Servant.Server
import Network.Wai.Handler.Warp
import qualified Todo
import TodoWeb
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM
import Control.Concurrent.STM.TVar

type Effect a = ReaderT (TVar Todo.Todos) Handler a

hoistEffect :: TVar Todo.Todos -> Effect a -> Handler a
hoistEffect todos rt = runReaderT rt todos

app :: TVar Todo.Todos -> Application
app todos = serve api $ hoistServer api (hoistEffect todos) server

main :: IO ()
main = do
  todos <- newTVarIO Todo.createNew
  run 9090 $ app todos
