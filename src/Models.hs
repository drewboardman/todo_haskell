module Models () where

import qualified Data.UUID as Uuid
import Data.UUID.V4 (nextRandom)
import qualified Data.Text as T
import qualified Data.Time.Clock as Time

newtype Content = Content T.Text deriving (Show, Eq) -- need ToJson if you add servant
type TodoID = Uuid.UUID

data Todo = 
  Pending { _content :: Content
          , _createdAt :: Time.UTCTime
          , _id :: TodoID
          } |
  Completed  { _content :: Content
             , _createdAt :: Time.UTCTime
             , _finishedAt :: Time.UTCTime
             , _id :: TodoID
             }

newTodo :: T.Text -> IO Todo
newTodo inputText = Pending (Content inputText) <$> Time.getCurrentTime <$> nextRandom
