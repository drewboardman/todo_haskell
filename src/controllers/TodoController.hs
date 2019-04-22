{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module TodoController (app) where

import           Control.Monad.Except   (join, throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.UUID              as Uuid
import qualified Models                 as M (AllTodos, Content, Pending, Todo,
                                              TodoID (TodoID))
import           Servant                ((:<|>) ((:<|>)), (:>), Application,
                                         Get, Handler, JSON, Post,
                                         Proxy (Proxy), QueryParam, ReqBody,
                                         Server, err404, serve)
import qualified TodoActions            as Actions (allTodos, getSingleTodo,
                                                    newTodo)

-- :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
type TodoAPI =
  -- GET: /todos/all -- returns all Todos
  "todos" :> "all" :> Get '[JSON] M.AllTodos
  -- GET: /todo/:uuid -- returns single Todo
  :<|> "todo" :> QueryParam "uuid" Uuid.UUID :> Get '[JSON] M.Todo
  -- POST: /todo/new -- creates pending Todo
  :<|> "todo" :> "new" :> ReqBody '[JSON] M.Content :> Post '[JSON] M.Pending

app :: Application
app = serve todoAPI todoServer

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

todoServer :: Server TodoAPI
todoServer = todos :<|> todo :<|> newTodo where

    todos :: Handler M.AllTodos
    todos = liftIO Actions.allTodos

    todo :: Maybe Uuid.UUID -> Handler M.Todo
    todo uuid = do
      maybeTodo <- liftIO $ traverse (Actions.getSingleTodo . M.TodoID) uuid
      case join maybeTodo of
        Just singleTodo -> return singleTodo
        Nothing         -> throwError err404

    -- how to validate Content?
    newTodo :: M.Content -> Handler M.Pending
    newTodo content = do
      maybeCreated <- liftIO $ Actions.newTodo content
      case maybeCreated of
        Just pending -> pure pending
        Nothing      -> throwError err404
