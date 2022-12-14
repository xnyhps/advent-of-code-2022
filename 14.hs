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

data Block = Sand | Rock
    deriving (Show)

isSand Sand = True
isSand Rock = False

parsePair :: Parser (Integer, Integer)
parsePair = do
    x <- decimal
    char ','
    y <- decimal
    return (x,y)

parseLine = sepBy1 parsePair (string " -> ")

makeRange ((sx,sy), (ex,ey)) | sx == ex = map (\y -> (sx, y)) $ enumFromTo (min sy ey) (max sy ey)
makeRange ((sx,sy), (ex,ey)) | sy == ey = map (\x -> (x, sy)) $ enumFromTo (min sx ex) (max sx ex)

makeLine segments = map makeRange (zip segments (drop 1 segments))

-- moveSand abyss (x,y) _ | y > abyss = Nothing
-- moveSand abyss (x,y) grid | Nothing <- DM.lookup (x, y + 1) grid = moveSand abyss (x, y + 1) grid
-- moveSand abyss (x,y) grid | Nothing <- DM.lookup (x - 1, y + 1) grid = moveSand abyss (x - 1, y + 1) grid
-- moveSand abyss (x,y) grid | Nothing <- DM.lookup (x + 1, y + 1) grid = moveSand abyss (x + 1, y + 1) grid
-- moveSand abyss (x,y) _ = Just (x,y)

moveSand abyss (x,y) _ | y == abyss - 1 = Just (x,y)
moveSand abyss (x,y) grid | Nothing <- DM.lookup (x, y + 1) grid = moveSand abyss (x, y + 1) grid
moveSand abyss (x,y) grid | Nothing <- DM.lookup (x - 1, y + 1) grid = moveSand abyss (x - 1, y + 1) grid
moveSand abyss (x,y) grid | Nothing <- DM.lookup (x + 1, y + 1) grid = moveSand abyss (x + 1, y + 1) grid
moveSand abyss (x,y) _ = Just (x,y)

run abyss grid = let
    newposition = moveSand abyss (500, 0) grid
    in case newposition of
        (Just (x,y)) | (x,y) == (500,0) -> DM.insert (x,y) Sand grid
        (Just (x,y)) -> run abyss (DM.insert (x,y) Sand grid)
        Nothing -> grid

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            let
                grid = DM.fromList (zip (concat (concatMap makeLine values)) (repeat Rock))
                abyss = maximum (map snd (concat values)) + 2
                result = run abyss grid

            print (length $ filter isSand $ map snd $ DM.toList result)