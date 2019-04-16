{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TodoController (app) where

import qualified Models  as M (Completed, Pending)
import           Servant ((:>), Application, Get, JSON, Proxy (Proxy), Server,
                          serve)

type TodoAPI =
  -- /todos/all -- returns all Todos
  "todos" :> "all" :> Get '[JSON] ([M.Pending], [M.Completed])

app :: Application
app = serve todoAPI todoServer

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

todoServer :: Server TodoAPI
todoServer = return ([], [])
