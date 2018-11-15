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
import Control.Monad.State
import qualified Data.Map.Strict as Map

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

list :: (MonadState Todos m) => m Todos
list = get

fetch :: (MonadError ServantErr m, MonadState Todo.Todos m) => Integer -> m Todo.Todo
fetch id = do
  ts <- get
  toM $ Map.lookup id ts
    where 
      toM (Just todo) = return todo
      toM Nothing = throwError err404

add :: (MonadState Todos m) => Integer -> Todo -> m Todo
add id todo = do
  modify (Map.insert id todo)
  return todo
