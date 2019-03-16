{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeApplications #-}

module DatabaseThings () where

import Database.Beam
import Database.Beam.Sqlite
import Data.Time.Clock as Time
import Models (Content(..), TodoID)

import Data.Text (Text)

-- is this Todo supposed to negate the one that I defined before?
-- should I just have this be called TodoTableEntry?
type Todo = TodoT Identity
type TodoTableID = PrimaryKey TodoT Identity

data TodoT f = Todo
  { _content :: Columnar f Content
  , _createdAt :: Columnar f Time.UTCTime
  , _finishedAt :: Columnar f Time.UTCTime
  , isPending :: Columnar f Bool
  , id :: TodoTableID } deriving (Generic)
