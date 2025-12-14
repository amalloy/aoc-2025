{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Control.Monad.Identity (Identity(..))
import Data.Maybe (fromMaybe)

import qualified Data.IntSet as S
import Data.IntSet (IntSet)

import Text.Regex.Applicative ((=~), (<|>), some, many, sym)
import Text.Regex.Applicative.Common (decimal)

type Joltage = Int
type Button = IntSet
data Machine = Machine IntSet [Button] [Joltage] deriving Show
type Input = [Machine]

toInts :: [Bool] -> IntSet
toInts = S.fromList . map fst . filter snd . zip [0..]

solutions :: Machine -> [IntSet]
solutions (Machine goal buttons _joltages) = map toInts . filter solved $ buttonChoices
  where buttonChoices = replicateM (length buttons) [False, True]
        solved :: [Bool] -> Bool
        solved = (== goal) . S.foldl' press S.empty . toInts
        press :: IntSet -> Int -> IntSet
        press lights buttonIdx = S.foldl' toggle lights (buttons !! buttonIdx)
        toggle :: IntSet -> Int -> IntSet
        toggle lights lightNum = runIdentity (S.alterF invert lightNum lights)
        invert :: Bool -> Identity Bool
        invert b = Identity (not b)

part1 :: Input -> Int
part1 = sum . map (minimum . map S.size . solutions)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some (machine <* sym '\n')
        machine = Machine <$>
          (lights <* sym ' ') <*>
          some (S.fromList <$> button <* sym ' ') <*>
          joltages
        lights = sym '[' *> (toInts <$> some light) <* sym ']'
        light = (True <$ sym '#') <|> (False <$ sym '.')
        ints open close = sym open *> (decimal `sepBy` sym ',') <* sym close
        joltages = ints '{' '}'
        button = ints '(' ')'
        p `sepBy` sep = (:) <$> p <*> many (sep *> p)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
