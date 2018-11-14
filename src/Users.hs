{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Users where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: UTCTime
}


