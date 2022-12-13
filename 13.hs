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
import Data.List
import qualified Data.Map.Strict as DM
import qualified Data.Set as DS
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Traversable (for)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

data Value = Number Integer | List [Value]
    deriving (Show, Eq)

compareValues (Number l) (Number r) | l < r  = LT
compareValues (Number l) (Number r) | l > r  = GT
compareValues (Number l) (Number r) | l == r = EQ
compareValues (List []) (List []) = EQ
compareValues (List []) _ = LT
compareValues _ (List []) = GT
compareValues (List (l:ls)) (List (r:rs)) | compareValues l r == LT = LT
compareValues (List (l:ls)) (List (r:rs)) | compareValues l r == GT = GT
compareValues (List (l:ls)) (List (r:rs)) | compareValues l r == EQ = compareValues (List ls) (List rs)
compareValues (List l) (Number r) = compareValues (List l) (List [Number r])
compareValues (Number l) (List r) = compareValues (List [Number l]) (List r)

instance Ord Value where
    compare = compareValues

parseNumber = Number <$> decimal

parseList = do
    char '['
    values <- sepBy parseSignal (char ',')
    char ']'
    return (List values)

parseSignal = parseNumber <|> parseList

parsePair = do
    l <- parseList
    char '\n'
    r <- parseList
    char '\n'
    return (l, r)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parsePair (char '\n')) input of
        Left err -> error err
        Right values -> do
            mapM_ print values

            -- let correct = filter (\(i, (l,r)) -> l < r) (zip [1..] values)

            -- mapM_ print correct

            -- print (sum (map fst correct))

            let
                extraTwo = List [List [Number 2]]
                extraSix = List [List [Number 6]]

                extras = [extraTwo, extraSix]
                sorted = sort ((concatMap (\(l,r) -> [l, r]) values) ++ extras)

                Just indexTwo = findIndex (== extraTwo) sorted
                Just indexSix = findIndex (== extraSix) sorted

            print ((indexTwo + 1) * (indexSix + 1))

