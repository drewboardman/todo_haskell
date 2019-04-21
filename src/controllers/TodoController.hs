{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module TodoController (app) where

import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromMaybe)
import qualified Data.UUID              as Uuid
import qualified Models                 as M (AllTodos, Todo, TodoID (TodoID))
import           Servant
-- import           Servant                    ((:<|>), (:>), Application, Get,
--                                              JSON, Proxy (Proxy), QueryParam,
--                                              ServantErr, Server, serve)
import qualified TodoActions            as Actions (allTodos, singleTodo)

type ServantMonad a b = ExceptT ServantErr a b

type TodoAPI =
  -- /todos/all -- returns all Todos
  "todos" :> "all" :> Get '[JSON] M.AllTodos
  -- /todo/:uuid -- returns single Todo
  :<|> "todo" :> QueryParam "uuid" Uuid.UUID :> Get '[JSON] M.Todo

app :: Application
app = serve todoAPI todoServer

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

todoServer :: Server TodoAPI
todoServer = todos :<|> todo where

    todos :: IO M.AllTodos
    todos = liftIO Actions.allTodos

    todo :: Uuid.UUID -> ServantMonad IO M.Todo
    todo uuid = do
      maybeTodo :: Maybe M.Todo <- Actions.singleTodo $ M.TodoID uuid
      fromMaybe (throwError "cannot find todo with the uuid") maybeTodo
