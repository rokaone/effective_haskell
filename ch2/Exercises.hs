module Exercises where
import Prelude hiding (foldl, foldr)

foldl func carryValue lst =
  if null lst     -- Base recursion case
  then carryValue -- Send the carryValue back up
  else foldl func (func carryValue (head lst)) (tail lst)   -- Pass the head of the list through each step

foldr func carryValue lst =
  if null lst     -- Base recursion case
  then carryValue -- Send the carryValue back up
  else func (head lst) $ foldr func carryValue (tail lst)   -- Pass the tail of the list through each step

-- Reverse a list using folds

reverseList lst = snd lst : (fst lst) : []

flipList a b = b : a

-- Zipping Lists

zip_comp func lst1 lst2 =
  let
    x1 = head lst1
    x2 = head lst2
    x1' = tail lst1
    x2' = tail lst2
  in if null lst1 || null lst2
    then []
    else (func x1 x2) : zip_comp func x1' x2'
  
