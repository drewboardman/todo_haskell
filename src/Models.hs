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

allTodos :: IO ([Pending], [Completed])
allTodos = do
  all <- Dao.allTodos
  splitTodos all

splitTodos :: [Dao.Todo] -> IO ([Pending], [Completed])
splitTodos todos = foldr splitter ([] :: [Pending], [] :: [Completed]) todos

splitter :: Dao.Todo -> ([Pending], [Completed]) -> ([Pending], [Completed])
splitter todo (pendings, completeds) =
  case todo of
    (Dao.Todo _ _ _ 1) -> (daoPendingToModels todo : pendings, completeds)
    (Dao.Todo _ _ _ 0) -> (pendings, fmap (:) (daoCompletedToModels todo) completeds)

daoPendingToModels :: Dao.Todo -> Pending
daoPendingToModels (Dao.Todo id content created _) = do
  uuid <- Uuid.fromText id
  Todo content created () uuid

-- daoCompletedToModels :: Dao.Todo -> Completed
-- daoCompletedToModels (Dao.Todo id text created _) = do
--   let ioTime :: IO Time.UTCTime = Time.getCurrentTime
--   let maybeUUID :: Maybe Uuid.UUID = Uuid.fromText id
--   bindedTime <- ioTime
--   bindedUuid <- maybeUUID
--   let completed :: CompletedTime = CompletedTime bindedTime
--   let uuid :: TodoID = TodoID bindedUuid
--   let todo = Todo (Content text) created completed uuid
--   todo

daoCompletedToModels :: Dao.Todo -> Completed
daoCompletedToModels (Dao.Todo id text created _) = do
  completed <- CompletedTime <$> Time.getCurrentTime
  uuid <- TodoID <$> Uuid.fromText id
  return $ Todo (Content text) created completed uuid
