module Models
  ( newTodo
  , completePendingTodo
  , Todo(..)
  , Content(..)
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

data Todo
  = Pending { _content   :: Content
            , _createdAt :: Time.UTCTime
            , _id        :: TodoID }
  | Completed { _content    :: Content
              , _createdAt  :: Time.UTCTime
              , _finishedAt :: Time.UTCTime
              , _id         :: TodoID }
  deriving (Eq, Show)

newTodo :: T.Text -> IO Todo
newTodo inputText = do
  time <- Time.getCurrentTime
  id <- nextRandom
  return $ Pending (Content inputText) time id

completePendingTodo :: Todo -> IO Todo
completePendingTodo todo = do
  time <- Time.getCurrentTime
  return $ todo { _finishedAt = time }
