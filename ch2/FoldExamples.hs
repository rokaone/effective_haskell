module FoldExamples where
import Prelude hiding (foldl, foldr)

foldl func carryValue lst =
  if null lst     -- Base recursion case
  then carryValue -- Send the carryValue back up
  else foldl func (func carryValue (head lst)) (tail lst)   -- Pass the head of the list through each step

foldr func carryValue lst =
  if null lst     -- Base recursion case
  then carryValue -- Send the carryValue back up
  else func (head lst) $ foldr func carryValue (tail lst)   -- Pass the tail of the list through each step

doubleElems = foldr doubleElem []
  where
    doubleElem num lst = (2 * num) : lst

doubleElems' elems = foldr (applyElem (*2)) [] elems
  where
    applyElem f elem accumulator = f elem : accumulator

map' f = foldr (applyElem f) []
  where
    applyElem f elem accumulator = f elem : accumulator

doubleWithMap elems = map' (*2) elems

map'' f xs =
  if null xs then []
  else f (head xs) : map'' f (tail xs)


