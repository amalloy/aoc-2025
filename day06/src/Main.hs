{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Applicative (ZipList(..))
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), some, many, sym, (<|>))
import Text.Regex.Applicative.Common (decimal)

type Operand = Int
data Operator = Plus | Times deriving (Eq, Ord, Show)

apply :: Operator -> (forall a. Num a => a -> a -> a)
apply Plus = (+)
apply Times = (*)

data ProblemF f = ProblemF (f (ZipList Operand)) (f Operator)
type Problem = ProblemF Identity
type Input = String

solve :: Applicative f => ProblemF f -> f Int
solve (ProblemF args ops) = go <$> args <*> ops
  where go arg op = foldl1 (apply op) arg

part1 :: Input -> Int
part1 sheet = sum . solve $ ProblemF args' ops
  where args' = sequenceA args
        (ProblemF args ops) = parse sheet
        parse = fromMaybe (error "no parse") . (=~ input)
          where p `sepBy` sep = opt sep *> ((:) <$> p <*> (many (sep *> p) <* opt sep))
                opt p = p <|> mempty
                input = ProblemF <$> (ZipList <$> some (row arg)) <*> (row op)
                row p = ZipList <$> ((p `sepBy` (some (sym ' '))) <* sym '\n')
                arg = decimal
                op = (Plus <$ sym '+') <|> (Times <$ sym '*')

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = id

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
