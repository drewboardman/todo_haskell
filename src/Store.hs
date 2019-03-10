module Store () where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Models

type Store = Map TodoID Todo

init :: Store
init = Map.empty

listPending :: Store -> [Todo]
