{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), many, sym, psym)
import qualified Data.Map.Lazy as M

data Label = Label Char Char Char deriving (Eq, Ord, Show)
data Node = Node {label :: Label, children :: NonEmpty Label} deriving (Show)

type Input = [Node]

you, out, svr, dac, fft :: Label
you = Label 'y' 'o' 'u'
out = Label 'o' 'u' 't'
svr = Label 's' 'v' 'r'
dac = Label 'd' 'a' 'c'
fft = Label 'f' 'f' 't'

part1 :: Input -> Int
part1 nodes = get you
  where get = (m M.!)
        m = M.insert out 1 . M.fromList $ do
          Node from tos <- nodes
          pure (from, sum . fmap get $ tos)

part2 :: Input -> Int
part2 nodes = get svr dac * get dac fft * get fft out + get svr fft * get fft dac * get dac out
  where get from to = m M.! (from, to)
        m = M.union outPaths normalNodes
        outPaths = M.fromList $ ((out, out), 1) : [((out, node), 0) | Node node _ <- nodes]
        normalNodes = M.fromList $ do
          (Node from children, to) <- (,) <$> nodes <*> (out : fmap label nodes)
          pure $ ((from, to), if from == to
            then 1
            else sum . fmap (`get` to) $ children)

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = many (node <* sym '\n')
        node = Node <$> (label <* sym ':') <*> some (sym ' ' *> label)
        label = Label <$> labelChar <*> labelChar <*> labelChar
        labelChar = psym (/= '\n')
        some p = (:|) <$> p <*> many p

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
