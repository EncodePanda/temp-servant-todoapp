{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Text
import Servant
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Class

class TodosStore m where
  getStore :: m Todos
  modifyStore :: (Todos -> Todos) -> m ()

instance (MonadIO m, MonadReader (TVar Todos) m) => TodosStore m where
  getStore = do
    tvar <- ask
    liftIO $ readTVarIO tvar
  modifyStore f = do
    tvar <- ask
    liftIO $ atomically $ modifyTVar tvar f

instance (Monad m, TodosStore m) => TodosStore (ExceptT e m) where
  getStore = lift getStore
  modifyStore f = lift $ modifyStore f

data Todo = Todo {
                   _title :: String
                 , _completed :: Bool
                 }
            deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

type Todos = Map.Map Integer Todo

createNew :: Todos
createNew = Map.fromList [(1, Todo "Learn Haskell" False), (2, Todo "Nothing else matters" True)]

list :: (TodosStore m) => m Todos
list = getStore

fetch :: (MonadError ServantErr m, TodosStore m) => Integer -> m Todo.Todo
fetch id = do
  ts <- getStore
  toM $ Map.lookup id ts
    where 
      toM (Just todo) = return todo
      toM Nothing = throwError err404

add :: (Monad m, TodosStore m) => Integer -> Todo -> m Todo
add id todo = do
  modifyStore (Map.insert id todo)
  return todo
