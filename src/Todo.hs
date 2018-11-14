{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Todo where

import Control.Monad.Trans.Except
import Data.Text
import Data.Time (UTCTime)
import Servant
import Data.Proxy
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Map.Strict as Map


-- domain
data Todo = Todo {
                   _title :: String
                 , _completed :: Bool
                 -- , _registration_date :: UTCTime
                 }
            deriving (Eq, Show, Generic)

instance ToJSON Todo

todos :: Map.Map Integer Todo
todos = Map.fromList [(1, Todo "Learn Haskell" False), (2, Todo "Nothing else matters" True)]

-- web

type TodoAPI 
    =    "todo" :> Get '[JSON] [Todo]
    :<|> "todo" :> Capture "id" Integer :> Get '[JSON] Todo

api :: Proxy TodoAPI
api = Proxy

fetchTodos :: Handler [Todo]
fetchTodos = return $ Map.elems todos

fetchTodo :: Integer -> Handler Todo
fetchTodo id = toHandler $ Map.lookup id todos
    where 
      toHandler :: Maybe Todo -> Handler Todo
      toHandler (Just todo) = return todo
      toHandler Nothing = throwError err404

server :: Server TodoAPI
server 
  =    fetchTodos
  :<|> fetchTodo
