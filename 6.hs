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
import Data.Maybe (catMaybes)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

findStartOfPacket x | length (nub (take 14 x)) == 14 = 14
findStartOfPacket (x:xs) | otherwise = 1 + findStartOfPacket xs
findStartOfPacket _ = error "Impossible"

main :: IO ()
main = do
    input <- readFile (replaceExtension $__FILE__ ".input")

    print input

    print (findStartOfPacket input)