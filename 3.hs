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

import Data.Attoparsec.Text hiding (take)

import Control.Monad (void, forM_, when)
import Control.Applicative ((<|>))
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List (transpose, permutations, intersect, nub)
import qualified Data.Map.Lazy as DM
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

parseLine = many1 letter

getPriority c | c >= 'a' && c <= 'z' = ord c + 1 - ord 'a'
getPriority c | c >= 'A' && c <= 'Z' = ord c + 27 - ord 'A'

findBadge [a, b, c] = nub (intersect c (intersect a b))

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            -- let priorities = map (\value -> let
            --         size = length value `div` 2
            --         lhs = take size value
            --         rhs = drop size value

            --         intersection = sum $ map getPriority $ nub $ intersect lhs rhs
            --         in intersection) values
            -- print (priorities)
            -- print (sum priorities)

            let groups = chunksOf 3 values
                badges = map (head . findBadge) groups

            print (sum (map getPriority badges))


