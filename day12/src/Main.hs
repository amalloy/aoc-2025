{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), (<|>), many, sym)
import Text.Regex.Applicative.Common (decimal)

import GHC.Generics (Generic1, Generically1(..))

data Three a = Three a a a deriving (Show, Foldable, Traversable, Functor, Generic1)
  deriving Applicative via Generically1 Three
newtype Nine a = Nine (Three (Three a)) deriving (Show, Foldable, Traversable, Functor, Generic1)
  deriving Applicative via Generically1 Nine
type Present = Nine Bool
data Problem = Problem Region [Int] deriving Show
type Region = (Int, Int)
data Input = Input [Present] [Problem] deriving Show

slack :: [Present] -> Problem -> Int
slack ps (Problem (h, w) cardinalities) = area - footprint
  where area = h * w
        footprint = sum $ zipWith (*) (map size ps) cardinalities
        size = length . filter id . toList p

part1 :: Input -> Int
part1 (Input shapes problems) = length . filter ((>= 0) . slack shapes) $ problems

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = Input <$> many (present <* sym '\n') <*> many (problem <* sym '\n')
        present = label *> (Nine <$> sequenceA (pure row))
        row = sequenceA (pure cell) <* sym '\n'
        cell = (True <$ sym '#') <|> (False <$ sym '.')
        label = (decimal @Int) *> sym ':' *> sym '\n'
        problem = Problem <$> region <*> many (sym ' ' *> decimal)
        region = (,) <$> (decimal <* sym 'x') <*> (decimal <* sym ':')

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
