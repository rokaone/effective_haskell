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
