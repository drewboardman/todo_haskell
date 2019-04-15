{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models
  ( newTodo
  , completePendingTodo
  , Todo(..)
  , Content(..)
  , Pending
  , Completed
  , TodoID
  ) where

import           Data.Aeson      ((.=), object, ToJSON, toJSON)
import qualified Data.Text       as T
import qualified Data.Time.Clock as Time
import qualified Data.UUID       as Uuid
import           Data.UUID.V4    (nextRandom)
import           GHC.Generics    (Generic)

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
