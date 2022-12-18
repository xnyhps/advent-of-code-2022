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

parseCoordinate = do
    x <- signed decimal
    char ','
    y <- signed decimal
    char ','
    z <- signed decimal
    return (x,y,z)

calculateFaces grid outside = DS.fold (\point score -> score + scorePoint point) 0 grid
    where
        scorePoint (x,y,z) = sum [if (x + dx, y + dy, z + dz) `DS.member` outside then 1 else 0 | (dx, dy, dz) <- directions]

directions =
    [ (1, 0, 0)
    , (-1, 0, 0)
    , (0, 1, 0)
    , (0, -1, 0)
    , (0, 0, 1)
    , (0, 0, -1)
    ]

fst3 (a,_,_) = a

findOutside :: DS.Set (Integer, Integer, Integer) -> DS.Set (Integer, Integer, Integer)
findOutside grid = DS.fromList (loop [(minx,miny,minz)] DS.empty)
        where

            values = DS.toList grid
            maxx = maximum (map (\(x,y,z) -> x) values) + 1
            minx = minimum (map (\(x,y,z) -> x) values) - 1
            maxy = maximum (map (\(x,y,z) -> y) values) + 1
            miny = minimum (map (\(x,y,z) -> y) values) - 1
            maxz = maximum (map (\(x,y,z) -> z) values) + 1
            minz = minimum (map (\(x,y,z) -> z) values) - 1

            loop ((x,y,z):queue) seen | (x,y,z) `DS.member` seen = loop queue seen
            loop ((x,y,z):queue) seen = let newPoints = step (x,y,z) in (x,y,z) : (loop (queue ++ newPoints) (DS.insert (x,y,z) seen))
            loop [] seen = []

            checkNeighbour (x,y,z) (dx,dy,dz) | x + dx < minx = Nothing
            checkNeighbour (x,y,z) (dx,dy,dz) | y + dy < miny = Nothing
            checkNeighbour (x,y,z) (dx,dy,dz) | z + dz < minz = Nothing
            checkNeighbour (x,y,z) (dx,dy,dz) | x + dx > maxx = Nothing
            checkNeighbour (x,y,z) (dx,dy,dz) | y + dy > maxy = Nothing
            checkNeighbour (x,y,z) (dx,dy,dz) | z + dz > maxz = Nothing
            checkNeighbour (x,y,z) (dx,dy,dz) | (x + dx, y + dy, z + dz) `DS.member` grid = Nothing
            checkNeighbour (x,y,z) (dx,dy,dz) | otherwise = Just (x + dx, y + dy, z + dz)

            step (x,y,z) = catMaybes (map (checkNeighbour (x,y,z)) directions)


main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseCoordinate (char '\n')) input of
        Left err -> error err
        Right values -> do
            let grid = DS.fromList values
                totalFaces = calculateFaces grid DS.empty
                outside = findOutside grid

            print (calculateFaces grid outside)
