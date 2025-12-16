{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Control.Monad.Identity (Identity(..))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.IntSet as I
import Data.IntSet (IntSet)
import qualified Data.Set as S
import Data.Set (Set)

import Text.Regex.Applicative ((=~), (<|>), some, many, sym)
import Text.Regex.Applicative.Common (decimal)

import Math.LinearEquationSolver (Solver(Z3), solveIntegerLinearEqsAll)

type Joltage = Int
type Button = IntSet
data Machine = Machine IntSet [Button] (IntMap Int) deriving Show
type Input = [Machine]

toInts :: [Bool] -> IntSet
toInts = I.fromList . map fst . filter snd . zip [0..]

part1 :: Input -> Int
part1 = sum . map (minimum . map I.size . solutions)
  where solutions :: Machine -> [IntSet]
        solutions (Machine goal buttons _joltages) = map toInts . filter solved $ buttonChoices
          where buttonChoices = replicateM (length buttons) [False, True]
                solved :: [Bool] -> Bool
                solved = (== goal) . I.foldl' press I.empty . toInts
                press :: IntSet -> Int -> IntSet
                press lights buttonIdx = I.foldl' toggle lights (buttons !! buttonIdx)
                toggle :: IntSet -> Int -> IntSet
                toggle lights lightNum = runIdentity (I.alterF invert lightNum lights)
                invert :: Bool -> Identity Bool
                invert b = Identity (not b)

part2 :: Input -> IO Int
part2 = fmap sum . traverse (uncurry solution) . zip [0..]
  where solution :: Int -> Machine -> IO Int
        solution machineIdx (Machine _lights buttons joltages) =
          let numResults = length joltages
              coeffs = (1 <$ buttons) : do
                r <- [0..numResults - 1]
                pure $ do
                  b <- buttons
                  pure . bool 0 1 $ r `I.member` b
              results = fromIntegral <$> M.elems joltages
              minPressesNeeded = fromIntegral $ maximum joltages
              maxPressesNeeded = fromIntegral $ sum joltages `div` (minimum . map I.size $ buttons)
              attempts = do
                totalPresses <- [minPressesNeeded..maxPressesNeeded]
                pure $ do
                  putStrLn $ "Trying " <> show totalPresses <> " buttons (giving up at " <> show maxPressesNeeded <> ")"
                  solveIntegerLinearEqsAll Z3 (fromIntegral $ totalPresses * 2) coeffs (totalPresses : results)
              findFirst [] = error "No solutions"
              findFirst (att:atts) = do
                try <- att
                case filter (all (>= 0)) try of
                  [] -> findFirst atts
                  (sol:_) -> pure . fromIntegral . sum $ sol
          in putStrLn ("Solving machine #" <> show machineIdx) *> findFirst attempts

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some (machine <* sym '\n')
        machine = Machine <$>
          (lights <* sym ' ') <*>
          some (I.fromList <$> button <* sym ' ') <*>
          joltages
        lights = sym '[' *> (toInts <$> some light) <* sym ']'
        light = (True <$ sym '#') <|> (False <$ sym '.')
        ints open close = sym open *> (decimal `sepBy` sym ',') <* sym close
        joltages = M.fromList . zip [0..] <$> ints '{' '}'
        button = ints '(' ')'
        p `sepBy` sep = (:) <$> p <*> many (sep *> p)

main :: IO ()
main = do
  input <- prepare <$> readFile "input.txt"
  print $ part1 input
  print =<< part2 input
