{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.Ord (Down(Down, getDown))

type Joltage = Int
type Bank = [Joltage]
type Input = [Bank]

dropLast :: Int -> [a] -> [a]
dropLast n xs = zipWith const xs (drop n xs)

maxJoltage :: Int -> Bank -> Int
maxJoltage _ [] = error "No batteries"
maxJoltage 1 bank = maximum bank
maxJoltage n bank = 10 * biggest + maxJoltage (n - 1) bank'
  where (biggest : bank') = drop skippedBatteries bank
        skippedBatteries = getDown . snd . maximum . (`zip` [Down 0, Down 1..]) $ dropLast (n - 1) bank

part1 :: Input -> Int
part1 = sum . map (maxJoltage 2)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (map digitToInt) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
