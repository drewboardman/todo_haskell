{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module TodoController (app) where

import           Control.Monad.IO.Class (liftIO)
import qualified Models                 as M (AllTodos, allTodos)
import           Servant                ((:>), Application, Get, JSON,
                                         Proxy (Proxy), Server, serve)

type TodoAPI =
  -- /todos/all -- returns all Todos
  "todos" :> "all" :> Get '[JSON] M.AllTodos

app :: Application
app = serve todoAPI todoServer

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

todoServer :: Server TodoAPI
todoServer = liftIO M.allTodos
