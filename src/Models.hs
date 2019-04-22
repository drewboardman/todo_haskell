{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Models
  ( AllTodos(AllTodos)
  , Todo(..)
  , Content(..)
  , TodoID(TodoID)
  , CompletedTime(CompletedTime)
  , Pending(Pending)
  , Completed(Completed)
  ) where

import           Data.Aeson      (FromJSON, ToJSON, object, toJSON, (.=))
import qualified Data.Text       as T
import qualified Data.Time.Clock as Time
import qualified Data.UUID       as Uuid
import           GHC.Generics    (Generic)

newtype TodoID = TodoID Uuid.UUID deriving (Eq, Show, Generic)
newtype Content = Content { contentText :: T.Text } deriving (Show, Eq, Generic)
newtype CompletedTime = CompletedTime Time.UTCTime deriving (Show, Eq, Generic)

data Todo = PendingTodo Pending | CompletedTodo Completed deriving (Eq, Show, Generic)

data Pending = Pending { _content     :: Content
                         , _createdAt :: Time.UTCTime
                         , _id        :: TodoID } deriving (Eq, Show, Generic)

data Completed = Completed { _content    :: Content
                           , _createdAt  :: Time.UTCTime
                           , _finishedAt :: CompletedTime
                           , _id         :: TodoID } deriving (Eq, Show, Generic)

data AllTodos = AllTodos { _pendings   :: [Pending]
                         , _completeds :: [Completed] } deriving (Eq, Show, Generic)

instance ToJSON Todo where
  toJSON (PendingTodo p)   = toJSON p
  toJSON (CompletedTodo c) = toJSON c

instance ToJSON AllTodos where
  toJSON (AllTodos ps cs) =
    object [ "pending_todos" .= ps
           , "completed_todos" .= cs ]

instance ToJSON Pending where
  toJSON (Pending c created uuid) =
    object [ "content" .= c
           , "created_at" .= created
           , "id" .= uuid ]

instance ToJSON Completed where
  toJSON (Completed content created finished uuid) =
    object [ "content" .= content
           , "created_at" .= created
           , "finished_at" .= finished
           , "id" .= uuid ]

instance ToJSON TodoID
instance ToJSON CompletedTime
instance ToJSON Content
instance FromJSON Content
