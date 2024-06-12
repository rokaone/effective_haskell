numbersStartingAt n =
  n : numbersStartingAt (n + 1)

epicCycle inputList =
  cycleHelper inputList
  where
    cycleHelper [] = epicCycle inputList
    cycleHelper (x:xs) = x : cycleHelper xs

moreEpicCycle inputList =
  inputList <> moreEpicCycle inputList

radsToDegrees :: Float -> Int
radsToDegrees radians =
  let degrees = cycle [0..359]
      converted = truncate $ (radians * 360) / (2 * pi)
  in degrees !! converted

