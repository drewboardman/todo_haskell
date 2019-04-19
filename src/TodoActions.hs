{-# LANGUAGE ScopedTypeVariables #-}

module TodoActions (singleTodo, allTodos, newTodo, completePendingTodo) where

import           Data.Either     (fromLeft, fromRight, isLeft, partitionEithers)
import           Data.Maybe      (catMaybes)
import qualified Data.Text       as T (Text)
import qualified Data.Time.Clock as Time
import qualified Data.UUID       as Uuid
import           Data.UUID.V4    (nextRandom)
import qualified Models          as M (AllTodos (AllTodos), Completed,
                                       CompletedTime (CompletedTime),
                                       Content (Content),
                                       GeneralTodo (CompletedTodo, PendingTodo),
                                       Pending, Todo (Todo), TodoID (TodoID))
import qualified TodoDao         as Dao (Todo, TodoT (Todo), allTodos,
                                         singleTodo)

newTodo :: T.Text -> IO M.Pending
newTodo inputText = do
  time <- Time.getCurrentTime
  M.Todo (M.Content inputText) time () . M.TodoID <$> nextRandom

completePendingTodo :: M.Pending -> IO M.Completed
completePendingTodo (M.Todo content created _ todoID) = do
  time <- Time.getCurrentTime
  return $ M.Todo content created (M.CompletedTime time) todoID

allTodos :: IO M.AllTodos
allTodos = do
  all' <- Dao.allTodos
  let partitioned = partitionEithers $ map toEither all'
  return $ M.AllTodos (catMaybes $ fst partitioned) (catMaybes $ snd partitioned)

singleTodo :: M.TodoID -> IO (Maybe M.GeneralTodo)
singleTodo (M.TodoID uuid) = do
  maybeFetched :: Maybe Dao.Todo <- Dao.singleTodo $ Uuid.toText uuid
  pure (intoGeneralTodo =<< maybeFetched)

intoGeneralTodo :: Dao.Todo -> Maybe M.GeneralTodo
intoGeneralTodo x = do
  let y = toEither x
  if isLeft y
    then M.PendingTodo <$> fromLeft Nothing y
    else M.CompletedTodo <$> fromRight Nothing y

toEither :: Dao.Todo -> Either (Maybe M.Pending) (Maybe M.Completed)
toEither todo = case todo of
    (Dao.Todo _ _ _ Nothing)  -> Left (daoPendingToModels todo)
    (Dao.Todo _ _ _ (Just _)) -> Right (daoCompletedToModels todo)

daoPendingToModels :: Dao.Todo -> Maybe M.Pending
daoPendingToModels (Dao.Todo id' content created _) =
  case Uuid.fromText id' of
    Just uuid -> Just $ M.Todo (M.Content content) created () (M.TodoID uuid)
    Nothing   -> Nothing

daoCompletedToModels :: Dao.Todo -> Maybe M.Completed
daoCompletedToModels (Dao.Todo id' text created maybeFinished) =
  case (Uuid.fromText id', maybeFinished) of
    (Just uuid, Just finished) ->
        Just $ M.Todo (M.Content text) created (M.CompletedTime finished) (M.TodoID uuid)
    (_, _) -> Nothing
