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

import qualified Data.Text       as T
import qualified Data.Time.Clock as Time
import qualified Data.UUID       as Uuid
import           Data.UUID.V4    (nextRandom)

newtype Content =
  Content T.Text
  deriving (Show, Eq) -- need ToJson if you add servant

type TodoID = Uuid.UUID

data Todo = PendingTodo Pending | CompletedTodo Completed

data Pending = Pending { _content   :: Content
                       , _createdAt :: Time.UTCTime
                       , _id        :: TodoID } deriving (Eq)

data Completed = Completed { _content    :: Content
                           , _createdAt  :: Time.UTCTime
                           , _finishedAt :: Time.UTCTime
                           , _id         :: TodoID } deriving (Eq)

newTodo :: T.Text -> IO Pending
newTodo inputText = do
  time <- Time.getCurrentTime
  Pending (Content inputText) time <$> nextRandom

completePendingTodo :: Pending -> IO Completed
completePendingTodo (Pending content created todoID) = do
  time <- Time.getCurrentTime
  return $ Completed content created time todoID
