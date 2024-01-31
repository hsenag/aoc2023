module Day1 where

import Data.Char (isDigit)

run :: IO ()
run =
  do
    input <- readFile "data/day1/input.txt"
    print $ go input
    print $ go2 input

go :: String -> Int
go = sum . map (select . map (read . (: [])) . filter isDigit) . lines

go2 :: String -> Int
go2 = sum . map (select . findDigits2) . lines

select :: [Int] -> Int
select [] = error "no numbers"
select [x] = x * 10 + x
select xs = head xs * 10 + last xs

findDigits2 :: String -> [Int]
findDigits2 (c : cs) | isDigit c = read [c] : findDigits2 cs
findDigits2 ('o' : cs@('n' : 'e' : _)) = 1 : findDigits2 cs
findDigits2 ('t' : cs@('w' : 'o' : _)) = 2 : findDigits2 cs
findDigits2 ('t' : cs@('h' : 'r' : 'e' : 'e' : _)) = 3 : findDigits2 cs
findDigits2 ('f' : cs@('o' : 'u' : 'r' : _)) = 4 : findDigits2 cs
findDigits2 ('f' : cs@('i' : 'v' : 'e' : _)) = 5 : findDigits2 cs
findDigits2 ('s' : cs@('i' : 'x' : _)) = 6 : findDigits2 cs
findDigits2 ('s' : cs@('e' : 'v' : 'e' : 'n' : _)) = 7 : findDigits2 cs
findDigits2 ('e' : cs@('i' : 'g' : 'h' : 't' : _)) = 8 : findDigits2 cs
findDigits2 ('n' : cs@('i' : 'n' : 'e' : _)) = 9 : findDigits2 cs
findDigits2 (_ : cs) = findDigits2 cs
findDigits2 [] = []
