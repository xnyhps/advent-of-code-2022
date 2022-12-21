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

doubleSplit x y list | x > y = doubleSplit y x list
doubleSplit x y list | x < y =
        let
            (start, end) = splitAt y list 
            (start1, start2) = splitAt x start
        in (start1, start2, end)

moveElement index distance list = let
    newPosition = (index + distance) `mod` (length list - 1)
    in case compare newPosition index of
        EQ -> list
        LT -> let
                (start, middle, (element:end)) = doubleSplit index (newPosition) list
            in start ++ [element] ++ middle ++ end
        GT -> let
                (start, (element:middle), end) = doubleSplit index (newPosition + 1) list
            in start ++ middle ++ [element] ++ end

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 (signed decimal) (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            let realValues = map (811589153 *) values

            let order = take (length values) [0..]

            let resultOrder = foldl' (\order (location, distance) -> let (Just index) = elemIndex location order in moveElement index distance order) order (concat $ replicate 10 $ zip order realValues)
                resultValues = map (realValues !!) resultOrder

            print resultValues

            let Just zero = elemIndex 0 resultValues 

            print (resultValues !! ((zero + 1000) `mod` length resultValues))
            print (resultValues !! ((zero + 2000) `mod` length resultValues))
            print (resultValues !! ((zero + 3000) `mod` length resultValues))


            print (resultValues !! ((zero + 1000) `mod` length resultValues) + resultValues !! ((zero + 2000) `mod` length resultValues) + resultValues !! ((zero + 3000) `mod` length resultValues))