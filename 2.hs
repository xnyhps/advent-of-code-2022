#!/usr/bin/env stack
{- stack
 runghc
 --package attoparsec
 --package filepath
 --package pseudomacros
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Attoparsec.Text

import Control.Monad (void, forM_, when)
import Control.Applicative ((<|>))
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List (transpose, permutations)
import qualified Data.Map.Lazy as DM

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

parseLine = do
    abc <- char 'A' <|> char 'B' <|> char 'C'
    char ' '
    xyz <- char 'X' <|> char 'Y' <|> char 'Z'

    return (abc, xyz)

-- shapeScore 'X' = 1
-- shapeScore 'Y' = 2
-- shapeScore 'Z' = 3

-- winnerScore 'A' 'X' = 3
-- winnerScore 'B' 'Y' = 3
-- winnerScore 'C' 'Z' = 3
-- winnerScore 'A' 'Y' = 6
-- winnerScore 'B' 'Z' = 6
-- winnerScore 'C' 'X' = 6
-- winnerScore _ _ = 0

-- score abc xyz = shapeScore xyz + winnerScore abc xyz

winnerScore 'X' = 0
winnerScore 'Y' = 3
winnerScore 'Z' = 6

shapeScore 'A' 'X' = 3
shapeScore 'A' 'Y' = 1
shapeScore 'A' 'Z' = 2

shapeScore 'B' 'X' = 1
shapeScore 'B' 'Y' = 2
shapeScore 'B' 'Z' = 3

shapeScore 'C' 'X' = 2
shapeScore 'C' 'Y' = 3
shapeScore 'C' 'Z' = 1

score abc xyz = shapeScore abc xyz + winnerScore xyz

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> print (sum (map (uncurry score) values))