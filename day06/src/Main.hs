{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Applicative (Alternative, ZipList(..), empty, optional)
import Data.Functor.Identity (Identity(..))
import Data.List (transpose)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative (RE, (=~), some, many, sym, (<|>))
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

op :: RE Char Operator
op = (Plus <$ sym '+') <|> (Times <$ sym '*')

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = optional sep *> ((:) <$> p <*> (many (sep *> p))) <* optional sep

part1 :: Input -> Int
part1 sheet = sum . solve $ ProblemF args' ops
  where args' = sequenceA args
        (ProblemF args ops) = parse sheet
        parse = fromMaybe (error "no parse") . (=~ input)
          where input = ProblemF <$> (ZipList <$> some (row arg)) <*> (row op)
                row p = ZipList <$> ((p `sepBy` (some (sym ' '))) <* sym '\n')
                arg = decimal

part2 :: Input -> Int
part2 sheet = sum . map (runIdentity . solve) $ problems
  where problems = parse . unlines . transpose . lines $ sheet
        parse = fromMaybe (error "no parse") . (=~ input)
        input = problem `sepBy` (spaces <* newline)
        problem = p <$> header <*> (some (arg <* newline))
          where p (arg1, op) args = ProblemF (Identity (ZipList (arg1:args))) (Identity op)
        header = (,) <$> arg <*> (op <* newline)
        arg = spaces *> decimal <* spaces
        spaces = many $ sym ' '
        newline = sym '\n'

prepare :: String -> Input
prepare = id

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
