module Store (listCompleted, listPending, initStore) where

import Data.Map (Map)
import qualified Data.Map as Map
import Models (Todo(PendingTodo, CompletedTodo), Pending, Completed, TodoID)

type Store = Map TodoID Todo

isPending :: Todo -> Bool
isPending (PendingTodo _) = True
isPending _ = False

isCompleted :: Todo -> Bool
isCompleted (CompletedTodo _) = True
isCompleted _ = False

initStore :: Store
initStore = Map.empty

listPending :: Store -> [Pending]
listPending store = pendings where
  pendings = [x | PendingTodo x <- Map.elems $ Map.filter isPending store]

listCompleted :: Store -> [Completed]
listCompleted store = completeds where
  completeds = [x | CompletedTodo x <- Map.elems $ Map.filter isCompleted store]
