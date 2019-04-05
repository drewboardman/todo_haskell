{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module DatabaseThings () where

-- import           Database.Beam.Sqlite
import qualified Data.Time.Clock as Time (UTCTime)
import           Database.Beam   (Columnar, Generic, Identity, PrimaryKey)
import           Models          (Content)

type Todo = TodoT Identity
type TodoTableID = PrimaryKey TodoT Identity

data TodoT f = Todo
  { _content    :: Columnar f Content
  , _createdAt  :: Columnar f Time.UTCTime
  , _finishedAt :: Columnar f Time.UTCTime
  , _isPending  :: Columnar f Bool
  , _id         :: Columnar f TodoTableID } deriving (Generic)
