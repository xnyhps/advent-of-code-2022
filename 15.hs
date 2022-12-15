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

parseLine = do
    string "Sensor at x="
    sx <- signed decimal
    string ", y="
    sy <- signed decimal
    string ": closest beacon is at x="
    bx <- signed decimal
    string ", y="
    by <- signed decimal
    return ((sx,sy),(bx,by))


manhattanDistance (sx,sy) (ex,ey) = abs (sx - ex) + abs (sy - ey)

type Intervals = [(Integer, Integer)]

findHole :: (Integer, Integer) -> Intervals -> Maybe Integer
findHole (start, end) intervals = go (sortBy (comparing fst) intervals) start
    where
        go [] cur | cur >= end = Nothing
        go [] cur | otherwise  = Just end
        go ((s,e):intervals) cur | s <= cur + 1 = go intervals (max e cur)
        go ((s,e):intervals) cur | otherwise    = Just (cur + 1)

makeInterval :: Integer -> ((Integer, Integer), Integer) -> Maybe (Integer, Integer)
makeInterval row ((x,y),distance) | abs (y - row) > distance = Nothing
makeInterval row ((x,y),distance) | otherwise = let deltay = distance - abs (y - row) in Just (x - deltay, x + deltay)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            let
                rowy = 11

                distances = map (\(s,b) -> (s, b, manhattanDistance s b)) values

                minx = minimum (map (\((sx,sy),(bx,by),mh) -> sx - mh) distances)
                maxx = maximum (map (\((sx,sy),(bx,by),mh) -> sx + mh) distances)
                

                becaons = DS.fromList (map snd values)

            -- print (minx, maxx)

            -- print distances

            -- let result = map (\rowx -> any (\((sx,sy),(bx,by),mh) -> (rowx,rowy) `DS.notMember` becaons && manhattanDistance (sx,sy) (rowx,rowy) <= mh) distances) (enumFromTo minx maxx)
            --     toString False = "."
            --     toString True = "#"

            -- print (zip (enumFromTo minx maxx) result)
            -- print (length (filter id result))

            -- let
            --     grid = concatMap (\x -> map (\y -> (x,y, (x,y) `DS.notMember` becaons && all (\((sx,sy),(bx,by),mh) -> manhattanDistance (sx,sy) (x,y) > mh) distances)) (enumFromTo 0 4000000)) (enumFromTo 0 4000000)
            --     (x,y,_) = head (filter (\(_,_,b) -> b) grid)

            -- print (x,y)
            -- print (x * 4000000 + y)

            let
                go row = let hole = findHole (0, 4000000) $ catMaybes (map (\((sx,sy),_,distance) -> makeInterval row ((sx,sy),distance)) distances) in case hole of
                    Nothing -> go (row + 1)
                    Just hole -> (row, hole, hole * 4000000 + row)

            print (go 0)