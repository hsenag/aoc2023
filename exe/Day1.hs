module Day1 where

import Data.Char (isDigit)

run :: IO ()
run =
  do
    input <- readFile "data/day1/input.txt"
    print $ go input

go :: String -> Int
go = sum . map (select . map (read . (: [])) . filter isDigit) . lines

select :: [Int] -> Int
select [] = error "no numbers"
select [x] = x * 10 + x
select xs = head xs * 10 + last xs
