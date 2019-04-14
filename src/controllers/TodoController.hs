{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TodoController () where

import qualified Models      as M (Todo)
import           Servant ((:>), Get, JSON)

type TodoAPI =
  -- /todos/all -- returns all Todos
  "todos" :> "all" :> Get '[JSON] [M.Todo]
