{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models
  ( newTodo
  , completePendingTodo
  , Todo(..)
  , Content(..)
  , Pending(..)
  , Completed(..)
  , TodoID
  ) where

import           Data.Aeson      (ToJSON)
import qualified Data.Text       as T
import qualified Data.Time.Clock as Time
import qualified Data.UUID       as Uuid
import           Data.UUID.V4    (nextRandom)
import           GHC.Generics    (Generic)

newtype TodoID = TodoID Uuid.UUID deriving (Eq, Show, Generic)
newtype Content = Content T.Text deriving (Show, Eq, Generic)
newtype CompletedTime = CompletedTime Time.UTCTime deriving (Show, Eq, Generic)

data Todo = PendingTodo Pending | CompletedTodo Completed deriving (Generic)

data Pending = Pending { _content   :: Content
                       , _createdAt :: Time.UTCTime
                       , _id        :: TodoID } deriving (Eq, Show, Generic)

data Completed = Completed { _content    :: Content
                           , _createdAt  :: Time.UTCTime
                           , _finishedAt :: CompletedTime
                           , _id         :: TodoID } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance ToJSON TodoID
instance ToJSON Completed
instance ToJSON CompletedTime
instance ToJSON Pending
instance ToJSON Content

newTodo :: T.Text -> IO Pending
newTodo inputText = do
  time <- Time.getCurrentTime
  Pending (Content inputText) time . TodoID <$> nextRandom

completePendingTodo :: Pending -> IO Completed
completePendingTodo (Pending content created todoID) = do
  time <- Time.getCurrentTime
  return $ Completed content created (CompletedTime time) todoID
