{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), many, sym, psym)
import qualified Data.Map.Lazy as M

data Label = Label Char Char Char deriving (Eq, Ord, Show)
data Node = Node Label (NonEmpty Label) deriving (Show)

type Input = [Node]

part1 :: Input -> Int
part1 nodes = get you
  where you = Label 'y' 'o' 'u'
        out = Label 'o' 'u' 't'
        get = (m M.!)
        m = M.insert out 1 . M.fromList $ do
          Node from tos <- nodes
          pure (from, sum . fmap get $ tos)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = many (node <* sym '\n')
        node = Node <$> (label <* sym ':') <*> some (sym ' ' *> label)
        label = Label <$> labelChar <*> labelChar <*> labelChar
        labelChar = psym (/= '\n')
        some p = (:|) <$> p <*> many p

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
