module Helpers (catBoth) where

import           Data.Maybe      (catMaybes)

catBoth :: ([Maybe a1], [Maybe a2]) -> ([a1], [a2])
catBoth (a1, a2) = (catMaybes a1, catMaybes a2)
