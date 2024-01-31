module Day2 where

import Control.Monad (void)
import Text.Parsec

run :: IO ()
run =
  do
    input <- readFile "data/day2/input.txt"
    print $ go input
    return ()

{-
Sample input:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
-}

data Game = Game
  { gameNumber :: Int
  , gameReveals :: [Reveal]
  }
  deriving (Eq, Show)

data Reveal = Reveal
  { revealList :: [BallCount]
  }
  deriving (Eq, Show)

data BallCount = BallCount
  { ballColour :: String
  , ballCount :: Int
  }
  deriving (Eq, Show)

-- a list of games is newline-separated
pInput :: Parsec String () [Game]
pInput = pGame `endBy` char '\n'

pGame :: Parsec String () Game
pGame =
  do
    void $ string "Game "
    n <- read <$> many1 digit
    void $ char ':'
    rs <- pReveal `sepBy` char ';'
    return $ Game n rs

pReveal :: Parsec String () Reveal
pReveal =
  do
    bs <- pBallCount `sepBy` char ','
    return $ Reveal bs

pBallCount :: Parsec String () BallCount
pBallCount =
  do
    void $ char ' '
    n <- read <$> many1 digit
    void $ char ' '
    c <- many1 letter
    return $ BallCount c n

-- a game is possible if all reveals contain no more than 12 red, 13 green and 14 blue
isPossible :: Game -> Bool
isPossible = all isPossibleReveal . gameReveals

isPossibleReveal :: Reveal -> Bool
isPossibleReveal = all isPossibleBallCount . revealList

isPossibleBallCount :: BallCount -> Bool
isPossibleBallCount (BallCount "red" n) = n <= 12
isPossibleBallCount (BallCount "green" n) = n <= 13
isPossibleBallCount (BallCount "blue" n) = n <= 14
isPossibleBallCount (BallCount colour _) = error $ "unknown colour: " ++ colour

-- result is the sum of the game numbers of all possible games
go :: String -> Int
go = sum . map gameNumber . filter isPossible . parseInput

parseInput :: String -> [Game]
parseInput = either (error . show) id . parse pInput ""

{- test data:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

-}

testData :: String
testData =
  unlines
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]