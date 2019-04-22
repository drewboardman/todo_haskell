{-# LANGUAGE ScopedTypeVariables #-}

module TodoActions (getSingleTodo, allTodos, newTodo, completePendingTodo) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Either            (partitionEithers)
import           Data.Maybe             (catMaybes)
import qualified Data.Time.Clock        as Time
import qualified Data.UUID              as Uuid
import           Data.UUID.V4           (nextRandom)
import qualified Models                 as M (AllTodos (AllTodos),
                                              Completed (Completed),
                                              CompletedTime (CompletedTime),
                                              Content (Content),
                                              Pending (Pending),
                                              Todo (CompletedTodo, PendingTodo),
                                              TodoID (TodoID))
import qualified TodoDao                as Dao (Todo, TodoT (Todo), allTodos,
                                                getSingleTodo,
                                                insertPendingTodo)

newTodo :: M.Content -> IO (Maybe M.Pending)
newTodo (M.Content contentText) = do
  time <- Time.getCurrentTime
  uuid <- nextRandom
  -- possibly unecessary to RE-convert to a pending?
  result <- liftIO (Dao.insertPendingTodo contentText time uuid)
  pure $ daoPendingToModels =<< result

completePendingTodo :: M.Pending -> IO M.Completed
completePendingTodo (M.Pending content created todoID) = do
  time <- Time.getCurrentTime
  return $ M.Completed content created (M.CompletedTime time) todoID

allTodos :: IO M.AllTodos
allTodos = do
  all' <- Dao.allTodos
  let partitioned = partitionEithers $ map eitherFromDaoTodo all'
  return $ M.AllTodos (catMaybes $ fst partitioned) (catMaybes $ snd partitioned)

getSingleTodo :: M.TodoID -> IO (Maybe M.Todo)
getSingleTodo (M.TodoID uuid) = do
  maybeFetched :: Maybe Dao.Todo <- Dao.getSingleTodo $ Uuid.toText uuid
  pure (toTodoM =<< maybeFetched)

toTodoM :: Dao.Todo -> Maybe M.Todo
toTodoM x = todoFromEither $ eitherFromDaoTodo x

todoFromEither :: Either (Maybe M.Pending) (Maybe M.Completed) -> Maybe M.Todo
todoFromEither (Left (Just pending))    = Just $ M.PendingTodo pending
todoFromEither (Left Nothing)           = Nothing
todoFromEither (Right (Just completed)) = Just $ M.CompletedTodo completed
todoFromEither (Right Nothing)          = Nothing

eitherFromDaoTodo :: Dao.Todo -> Either (Maybe M.Pending) (Maybe M.Completed)
eitherFromDaoTodo todo = case todo of
    (Dao.Todo _ _ _ Nothing)  -> Left (daoPendingToModels todo)
    (Dao.Todo _ _ _ (Just _)) -> Right (daoCompletedToModels todo)

daoPendingToModels :: Dao.Todo -> Maybe M.Pending
daoPendingToModels (Dao.Todo id' content created _) =
  case Uuid.fromText id' of
    Just uuid -> Just $ M.Pending (M.Content content) created (M.TodoID uuid)
    Nothing   -> Nothing

daoCompletedToModels :: Dao.Todo -> Maybe M.Completed
daoCompletedToModels (Dao.Todo id' content created maybeFinished) =
  case (Uuid.fromText id', maybeFinished) of
    (Just uuid, Just finished) -> Just $
        M.Completed (M.Content content) created (M.CompletedTime finished) (M.TodoID uuid)
    (_, _) -> Nothing
