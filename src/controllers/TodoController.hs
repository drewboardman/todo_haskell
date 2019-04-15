{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TodoController () where

import qualified Models      as M (Pending, Completed)
import           Servant ((:>), Get, JSON)

type TodoAPI =
  -- /todos/all -- returns all Todos
  "todos" :> "all" :> Get '[JSON] ([M.Pending], [M.Completed])
