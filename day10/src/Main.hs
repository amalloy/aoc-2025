module Main where

import Control.Monad (replicateM, guard)
import Control.Monad.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.Traversable (for)

import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.IntSet as I
import Data.IntSet (IntSet)

import Z3.Monad

import Text.Regex.Applicative ((=~), (<|>), some, many, sym)
import Text.Regex.Applicative.Common (decimal)

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
        solution _machineIdx (Machine _lights buttons joltages) = do
            (Just total, Just _presses) <- evalZ3 program
            -- print (machineIdx, total, presses)
            -- putStrLn =<< evalZ3 (program *> solverToString)
            pure . fromIntegral $ total
          where program = do
                  _0 <- mkInteger 0
                  buttonVars <- for (zip [0::Int ..] buttons) $ \(n, _b) -> do
                    b <- mkFreshIntVar $ "b" <> show n
                    optimizeAssert =<< mkGe b _0
                    pure b
                  let buttonsAffecting n = do
                        (b, v) <- zip buttons buttonVars
                        guard $ n `I.member` b
                        pure v
                  totalPresses <- mkFreshIntVar "total"
                  optimizeAssert =<< mkEq totalPresses =<< mkAdd buttonVars
                  optimizeMinimize totalPresses
                  for_ (M.assocs joltages) $ \(n, j) -> do
                    goal <- mkInteger (fromIntegral j)
                    optimizeAssert =<< mkEq goal =<< mkAdd (buttonsAffecting n)
                  optimizeCheck [] -- ?????
                  model <- optimizeGetModel
                  total <- evalInt model totalPresses
                  presses <- traverse (evalInt model) buttonVars
                  pure (total, sequenceA presses)

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
