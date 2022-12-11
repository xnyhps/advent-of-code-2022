#!/usr/bin/env stack
{- stack
 runghc
 --package attoparsec
 --package filepath
 --package pseudomacros
 --package split
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Attoparsec.Text hiding (take, takeWhile, D)

import Control.Monad (void, forM_, when)
import Control.Applicative ((<|>))
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List (transpose, permutations, intersect, nub, tails, sortBy, sort)
import qualified Data.Map.Strict as DM
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

data Value = Old | Constant Integer
    deriving (Show)

data Operation = Add Value Value | Multiply Value Value
    deriving (Show)

data Monkey = Monkey {
    items :: [Integer],
    operation :: Operation,
    divisorTest :: Integer,
    trueId :: Integer,
    falseId :: Integer
} deriving (Show)

parseValue = ((Old <$ string "old") <|> (Constant <$> decimal)) <?> "parseValue"

parseAddition = do
    l <- parseValue
    string " + "
    r <- parseValue
    return (Add l r)

parseMultiplication = do
    l <- parseValue
    string " * "
    r <- parseValue
    return (Multiply l r)

parseOperation = (parseMultiplication <|> parseAddition) <?> "parseOperation"

parseMonkey = do
    string "Monkey " <?> "Monkey"
    number <- decimal
    string ":\n  Starting items: " <?> "Starting items"
    items <- sepBy decimal (string ", ")
    string "\n  Operation: new = " <?> "Operation"
    operation <- parseOperation
    string "\n  Test: divisible by " <?> "Test"
    divisor <- decimal
    string "\n    If true: throw to monkey " <?> "True"
    trueId <- decimal
    string "\n    If false: throw to monkey " <?> "False"
    falseId <- decimal
    return (number, Monkey items operation divisor trueId falseId)

evaluateValue Old item = item
evaluateValue (Constant c) _ = c

evaluate (Add l r) item = evaluateValue l item + evaluateValue r item
evaluate (Multiply l r) item = evaluateValue l item * evaluateValue r item

performStep :: Integer -> Integer -> Monkey -> (DM.Map Integer Monkey, DM.Map Integer Integer) -> Integer -> (DM.Map Integer Monkey, DM.Map Integer Integer)
performStep bigDivisor number (Monkey {..}) (monkeys, stats) item = let
        newvalue = (evaluate operation item) `mod` bigDivisor
        newowner = if newvalue `mod` divisorTest == 0 then trueId else falseId

        monkeyAddItem item (Just (Monkey {..})) = Just (Monkey { items = items ++ [item], .. })

    in (DM.insert number (Monkey { items = [], .. }) (DM.alter (monkeyAddItem newvalue) newowner monkeys), DM.adjust (+ 1) number stats)

performRound :: Integer -> (DM.Map Integer Monkey, DM.Map Integer Integer) -> [Integer] -> (DM.Map Integer Monkey, DM.Map Integer Integer)
performRound _ monkeys [] = monkeys
performRound bigDivisor (monkeys, stats) (number:numbers) | Just m <- DM.lookup number monkeys = performRound bigDivisor (foldl (performStep bigDivisor number m) (monkeys, stats) (items m)) numbers


main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseMonkey (string "\n\n")) input of
        Left err -> error err
        Right values -> do
            let
                monkeys :: DM.Map Integer Monkey
                monkeys = DM.fromList values
                initialStats = DM.fromList (take (length values) (zip [0..] (repeat 0)))
                bigDivisor = product (map (divisorTest . snd) values)
                (result, stats) = foldl (\(monkeys, stats) _ -> performRound bigDivisor (monkeys, stats) (sort (DM.keys monkeys))) (monkeys, initialStats) [1..10000]

            print (product $ take 2 $ reverse $ sort $ DM.elems stats)
