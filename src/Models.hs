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
  , Pending
  , GeneralTodo(CompletedTodo, PendingTodo)
  , Completed
  , TodoID(TodoID)
  , CompletedTime(CompletedTime)
  ) where

import           Data.Aeson      (ToJSON, object, toJSON, (.=))
import qualified Data.Text       as T
import qualified Data.Time.Clock as Time
import qualified Data.UUID       as Uuid
import           GHC.Generics    (Generic)

newtype TodoID = TodoID Uuid.UUID deriving (Eq, Show, Generic)
newtype Content = Content T.Text deriving (Show, Eq, Generic)
newtype CompletedTime = CompletedTime Time.UTCTime deriving (Show, Eq, Generic)

data Todo completedTime = Todo { _content    :: Content
                               , _createdAt  :: Time.UTCTime
                               , _finishedAt :: completedTime
                               , _id         :: TodoID } deriving (Eq, Show, Generic)

data AllTodos = AllTodos { _pendings   :: [Pending]
                         , _completeds :: [Completed] } deriving (Eq, Show, Generic)

data GeneralTodo = PendingTodo Pending | CompletedTodo Completed

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
