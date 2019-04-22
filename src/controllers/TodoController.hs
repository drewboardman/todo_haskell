{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module TodoController (app) where

import           Control.Monad.Except   (join, throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.UUID              as Uuid
import qualified Models                 as M (AllTodos, Todo, TodoID (TodoID))
import           Servant                ((:<|>) ((:<|>)), (:>), Application,
                                         Get, Handler, JSON, Proxy (Proxy),
                                         QueryParam, Server, err404, serve)
import qualified TodoActions            as Actions (allTodos, singleTodo)

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

    todos :: Handler M.AllTodos
    todos = liftIO Actions.allTodos

    todo :: Maybe Uuid.UUID -> Handler M.Todo
    todo uuid = do
      maybeTodo <- liftIO $ traverse (Actions.singleTodo . M.TodoID) uuid
      thing $ join maybeTodo

-- thing :: (Monad m, MonadError ServantErr m, IsString e) => Maybe M.Todo -> m M.Todo
thing ::  Maybe M.Todo -> Handler M.Todo
thing x = case x of
  Just todo -> return todo
  Nothing   -> throwError err404
