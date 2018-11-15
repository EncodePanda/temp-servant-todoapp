{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module TodoWeb where

import qualified Todo
import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Text
import Data.Time (UTCTime)
import Servant
import Data.Proxy
import Data.Aeson.Types
import GHC.Generics
import Control.Monad.State
import qualified Data.Map.Strict as Map

type TodoAPI 
    =    "todo" :> Get '[JSON] (Map.Map Integer Todo.Todo)
    :<|> "todo" :> Capture "id" Integer :> Get '[JSON] Todo.Todo
    :<|> "todo" :> Capture "id" Integer :> ReqBody '[JSON] Todo.Todo :> Put '[JSON] Todo.Todo

api :: Proxy TodoAPI
api = Proxy

server :: (MonadError ServantErr m, MonadState Todo.Todos m) => ServerT TodoAPI m
server
  =    Todo.list
  :<|> Todo.fetch
  :<|> Todo.add

