{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

import qualified Data.IntSet as S

import Text.Regex.Applicative ((=~), (<|>), some, many, sym)
import Text.Regex.Applicative.Common (decimal)

type Joltage = Int
type Button = S.IntSet
data Machine = Machine [Bool] [Button] [Joltage] deriving Show
type Input = [Machine]

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some (machine <* sym '\n')
        machine = Machine <$>
          (lights <* sym ' ') <*>
          some (S.fromList <$> button <* sym ' ') <*>
          joltages
        lights = sym '[' *> some light <* sym ']'
        light = (True <$ sym '#') <|> (False <$ sym '.')
        ints open close = sym open *> (decimal `sepBy` sym ',') <* sym close
        joltages = ints '{' '}'
        button = ints '(' ')'
        p `sepBy` sep = (:) <$> p <*> many (sep *> p)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
