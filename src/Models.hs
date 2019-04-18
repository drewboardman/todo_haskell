{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Models
  ( newTodo
  , completePendingTodo
  , Todo(..)
  , Content(..)
  , Pending
  , Completed
  , TodoID
  ) where

import           Data.Aeson      (ToJSON, object, toJSON, (.=))
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

type Pending = Todo ()
type Completed = Todo CompletedTime

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

-- allTodos :: IO ([Pending], [Completed])
-- allTodos = do
--   all <- Dao.allTodos
--   splitTodos all

-- splitTodos :: [Dao.Todo] -> IO ([Pending], [Completed])
-- splitTodos todos = foldr splitter ([] :: [Pending], [] :: [Completed]) todos

toEithers :: [Dao.Todo] -> IO [Either (Maybe Pending) (Maybe Completed)]
toEithers todos = mapM splitter todos

splitter :: Dao.Todo -> IO (Either (Maybe Pending) (Maybe Completed))
splitter todo = case todo of
    (Dao.Todo _ _ _ Nothing) -> return $ fmap convertPending (daoPendingToModels todo)
    (Dao.Todo _ _ _ (Just _)) -> return $ fmap convertCompleted (daoCompletedToModels todo)

convertPending :: Maybe Pending -> Either (Maybe Pending) (Maybe Completed)
convertPending x = Left x :: Either (Maybe Pending) (Maybe Completed)

convertCompleted :: Maybe Completed -> Either (Maybe Pending) (Maybe Completed)
convertCompleted x = Right x :: Either (Maybe Pending) (Maybe Completed)

daoPendingToModels :: Dao.Todo -> IO (Maybe Pending)
daoPendingToModels (Dao.Todo id' content created _) =
  case Uuid.fromText id' of
    Just uuid -> pure $ Just $ Todo (Content content) created () (TodoID uuid)
    Nothing -> pure Nothing

daoCompletedToModels :: Dao.Todo -> IO (Maybe Completed)
daoCompletedToModels (Dao.Todo id' text created maybeFinished) =
  case (Uuid.fromText id', maybeFinished) of
    (Just uuid, Just finished) -> pure completedTodo where
      completedTodo = -- make this look less like shit
        Just $ Todo (Content text) created (CompletedTime finished) (TodoID uuid)
    (_, _) -> pure Nothing
