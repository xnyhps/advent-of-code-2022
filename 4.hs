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

parseLine = do
    lst <- decimal
    char '-'
    lend <- decimal
    char ','
    rst <- decimal
    char '-'
    rend <- decimal
    return ((lst, lend), (rst, rend))

isIncluded ((lst, lend), (rst, rend)) | lst >= rst && lend <= rend = True
isIncluded ((lst, lend), (rst, rend)) | rst >= lst && rend <= lend = True
isIncluded _ | otherwise = False

isOverlapping x | isIncluded x = True
isOverlapping ((lst, lend), (rst, rend)) | lst >= rst && lst <= rend = True
isOverlapping ((lst, lend), (rst, rend)) | rst >= lst && rst <= lend = True
isOverlapping _ | otherwise = False

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            -- let included = filter isIncluded values

            -- print included
            -- print (length included)

            let overlapped = filter isOverlapping values

            print overlapped
            print (length overlapped)