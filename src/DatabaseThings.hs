{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module DatabaseThings () where

-- import           Database.Beam.Sqlite
import           Data.Time.Clock as Time (UTCTime)
import           Database.Beam   (Columnar, Generic, Identity, PrimaryKey)
import           Models          (Content)

-- is this Todo supposed to negate the one that I defined before?
-- should I just have this be called TodoTableEntry?
type Todo = TodoT Identity
type TodoTableID = PrimaryKey TodoT Identity

data TodoT f = Todo
  { _content    :: Columnar f Content
  , _createdAt  :: Columnar f Time.UTCTime
  , _finishedAt :: Columnar f Time.UTCTime
  , isPending   :: Columnar f Bool
  , id          :: TodoTableID } deriving (Generic)
