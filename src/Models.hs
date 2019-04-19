{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Models
  ( newTodo
  , AllTodos(AllTodos)
  , completePendingTodo
  , Todo(..)
  , Content(..)
  , Pending
  , Completed
  , TodoID
  , allTodos
  ) where

import           Data.Aeson      (ToJSON, object, toJSON, (.=))
import           Data.Either     (partitionEithers)
import           Data.Maybe      (catMaybes)
import qualified Data.Text       as T
import qualified Data.Time.Clock as Time
import qualified Data.UUID       as Uuid
import           Data.UUID.V4    (nextRandom)
import           GHC.Generics    (Generic)
import qualified TodoDao         as Dao (Todo, TodoT (Todo), allTodos)

newtype TodoID = TodoID Uuid.UUID deriving (Eq, Show, Generic)
newtype Content = Content T.Text deriving (Show, Eq, Generic)
newtype CompletedTime = CompletedTime Time.UTCTime deriving (Show, Eq, Generic)

data Todo completedTime = Todo { _content    :: Content
                               , _createdAt  :: Time.UTCTime
                               , _finishedAt :: completedTime
                               , _id         :: TodoID } deriving (Eq, Show, Generic)

data AllTodos = AllTodos { _pendings   :: [Pending]
                         , _completeds :: [Completed] } deriving (Eq, Show, Generic)

type Pending = Todo ()
type Completed = Todo CompletedTime

instance ToJSON AllTodos where
  toJSON (AllTodos ps cs) =
    object [ "pending_todos" .= ps
           , "completed_todos" .= cs ]

instance ToJSON Pending where
  toJSON (Todo c created _ uuid) =
    object [ "content" .= c
           , "created_at" .= created
           , "id" .= uuid ]

instance ToJSON Completed where
  toJSON (Todo c created finished uuid) =
    object [ "content" .= c
           , "created_at" .= created
           , "finished_at" .= finished
           , "id" .= uuid ]

instance ToJSON TodoID
instance ToJSON CompletedTime
instance ToJSON Content

newTodo :: T.Text -> IO Pending
newTodo inputText = do
  time <- Time.getCurrentTime
  Todo (Content inputText) time () . TodoID <$> nextRandom

completePendingTodo :: Pending -> IO Completed
completePendingTodo (Todo content created _ todoID) = do
  time <- Time.getCurrentTime
  return $ Todo content created (CompletedTime time) todoID

allTodos :: IO AllTodos
allTodos = do
  all' <- Dao.allTodos
  let partitioned = partitionEithers $ map toEither all'
  return $ AllTodos (catMaybes $ fst partitioned) (catMaybes $ snd partitioned)

toEither :: Dao.Todo -> Either (Maybe Pending) (Maybe Completed)
toEither todo = case todo of
    (Dao.Todo _ _ _ Nothing)  -> Left (daoPendingToModels todo)
    (Dao.Todo _ _ _ (Just _)) -> Right (daoCompletedToModels todo)

daoPendingToModels :: Dao.Todo -> Maybe Pending
daoPendingToModels (Dao.Todo id' content created _) =
  case Uuid.fromText id' of
    Just uuid -> Just $ Todo (Content content) created () (TodoID uuid)
    Nothing   -> Nothing

daoCompletedToModels :: Dao.Todo -> Maybe Completed
daoCompletedToModels (Dao.Todo id' text created maybeFinished) =
  case (Uuid.fromText id', maybeFinished) of
    (Just uuid, Just finished) ->
        Just $ Todo (Content text) created (CompletedTime finished) (TodoID uuid)
    (_, _) -> Nothing
