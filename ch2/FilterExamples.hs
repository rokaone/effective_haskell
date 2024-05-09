module FilterExamples where
import Prelude hiding(foldl, foldr)

foldl func carryValue lst =
  if null lst     -- Base recursion case
  then carryValue -- Send the carryValue back up
  else foldl func (func carryValue (head lst)) (tail lst)   -- Pass the head of the list through each step

foldr func carryValue lst =
  if null lst     -- Base recursion case
  then carryValue -- Send the carryValue back up
  else func (head lst) $ foldr func carryValue (tail lst)   -- Pass the tail of the list through each step

checkGuestList guestList name =
  name `elem` guestList

foodCosts =
  [("Ren", 10.00)
  ,("George", 4.00)
  ,("Porter", 27.50)]

partyBudget isAttending =
  foldr (+) 0 . map snd . filter (isAttending . fst)

pairs as bs =
  let as' = filter (`elem` bs) as
      bs' = filter odd bs
      mkPairs a = map ( \b -> (a,b)) bs'
  in concat $ map mkPairs as'

combineLists as bs =
  let
    a = head as
    b = head bs
    as' = tail as
    bs' = tail bs
  in if null as ||  null bs
    then []
    else (a,b) : combineLists as' bs'

pairwiseSum xs ys =
  let sumElems pairs = 
        let a = fst pairs
            b = snd pairs
        in a + b
  in map sumElems $ zip xs ys

