module Exercises where

-- Factorial
-- factorial 5 = (5 * 4 * 3 * 2 * 1) = 120

factorialFor res num = res * num

factorial currNum factorialOutput =
  if currNum == 1
  then show factorialOutput
  else
    let nextOutput = factorialFor factorialOutput currNum 
        nextNum = currNum - 1
    in factorial nextNum nextOutput

-- Fibonacci        0 1 2 3 4 5 6 7
-- fibonacci seq -> 0 1 1 2 3 5 8 13 ...

fibonacci fibIndex currIndex fibCurr fibPrev =
  if currIndex == fibIndex
  then show fibCurr
  else
    let fibNext = fibCurr + fibPrev
        nextIndex = currIndex + 1
    in fibonacci fibIndex nextIndex fibNext fibCurr
