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

import Data.Attoparsec.Text hiding (take, takeWhile)

import Control.Monad (void, forM_, when)
import Control.Applicative ((<|>))
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List (transpose, permutations, intersect, nub, tails, sortBy)
import qualified Data.Map.Lazy as DM
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

readDigit :: Char -> Integer
readDigit c = read [c]

parseTrees :: Parser [[Integer]]
parseTrees = sepBy1 (map readDigit <$> many1 digit) (char '\n')

treesVisible value [] = 0
treesVisible value (x:xs) | x >= value = 1
treesVisible value (x:xs) | otherwise = 1 + treesVisible value xs

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly parseTrees input of
        Left err -> error err
        Right values -> do
            print values

            let width = length (head values)
                height = length values
                grid = DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((row, column), value))) [0..] $ map (zip [0..]) values

            print grid

            -- let result =
            --         flip map [0..width-1] $ \x ->
            --             flip map [0..height-1] $ \y ->
            --                 let value = DM.findWithDefault (error ("huh" ++ show (x,y))) (x,y) grid
            --                     left = map (\x_ -> (DM.findWithDefault (-1) (x_,y) grid)) [-1..x-1]
            --                     right = map (\x_ -> (DM.findWithDefault (-1) (x_,y) grid)) [x+1..width]

            --                     up = map (\y_ -> (DM.findWithDefault (-1) (x,y_) grid)) [-1..y-1]
            --                     down = map (\y_ -> (DM.findWithDefault (-1) (x,y_) grid)) [y+1..height]

            --                 in any id [all (< value) left, all (< value) right, all (< value) up, all (< value) down]

            -- print (length (filter id (concat result)))

            let result =
                    flip map [0..width-1] $ \x ->
                        flip map [0..height-1] $ \y ->
                            let value = DM.findWithDefault (error ("huh" ++ show (x,y))) (x,y) grid
                                left = map (\x_ -> (DM.findWithDefault (error "huh") (x_,y) grid)) [x-1,x-2..0]
                                right = map (\x_ -> (DM.findWithDefault (error "huh") (x_,y) grid)) [x+1..width-1]

                                up = map (\y_ -> (DM.findWithDefault (error "huh") (x,y_) grid)) [y-1,y-2..0]
                                down = map (\y_ -> (DM.findWithDefault (error "huh") (x,y_) grid)) [y+1..height-1]

                            in product (map (treesVisible value) [left, right, up, down])

            print result
            print (maximum (concat result))