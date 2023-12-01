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

data Direction = L | R
    deriving (Show)

type Piece = [(Integer, Integer)]

type Grid = DS.Set (Integer, Integer)

horizontalLine :: Piece
horizontalLine = [(0,0), (1,0), (2,0), (3,0)]

plus :: Piece
plus = [(1,0), (0,1), (1,1), (2,1), (1,2)]

l :: Piece
l = [(0,0), (1,0), (2,0), (2,1), (2,2)]

verticalLine :: Piece
verticalLine = [(0,0), (0,1), (0,2), (0,3)]

square :: Piece
square = [(0,0), (1,0), (0,1), (1,1)]

shapes = [horizontalLine, plus, l, verticalLine, square]

parseDirection = (L <$ char '<') <|> (R <$ char '>')

movePiece (dx,dy) piece = map (\(x,y) -> (x + dx, y + dy)) piece

collides :: Piece -> Grid -> Bool
collides piece grid = any (\(x,y) -> DS.member (y,x) grid) piece

updateGrid :: Piece -> Grid -> Grid
updateGrid piece grid = foldl (\grid (x,y) -> DS.insert (y,x) grid) grid piece

run :: Piece -> Grid -> [Direction] -> Int -> (Grid, Int)
run piece grid winds windIdx = let
    startHeight = fst (DS.findMax grid) + 4
    placedPiece = movePiece (2,startHeight) piece
    in applyWind placedPiece windIdx
    where
        applyWind piece windIdx = let
                wind = winds !! windIdx
                (deltax, bound) = case wind of
                    L -> (-1, 0)
                    R -> (1, 6)
                newPosition = if any (\(x,y) -> x == bound) piece || collides (movePiece (deltax, 0) piece) grid then piece else movePiece (deltax, 0) piece
            in applyGravity newPosition ((windIdx + 1) `mod` length winds)
        
        applyGravity piece windIdx | collides (movePiece (0, -1) piece) grid = (updateGrid piece grid, windIdx)
        applyGravity piece windIdx | otherwise = applyWind (movePiece (0, -1) piece) windIdx

totalIterations = 1000000000000

go iterations grid _ _ _ _ _ | iterations == totalIterations - 1 = maximum $ map fst $ DS.toList grid
go iterations grid pieces pieceIdx winds windIdx seen = let
    piece = pieces !! pieceIdx
    (resultGrid, resultWindIdx) = run piece grid winds windIdx
    heightResultGrid = maximum $ map fst $ DS.toList resultGrid
    heightMap = [heightResultGrid - (maximum $ map fst $ filter (\(x,y) -> y == a) $ DS.toList resultGrid) | a <- [0..6]]
    nextPieceIdx = (pieceIdx + 1) `mod` length pieces
    in case DM.lookup (pieceIdx, windIdx, heightMap) seen of
        Just x@(previousIterations, previousHeight) ->
            let
                period = iterations - previousIterations
                remainingLoops = (totalIterations - iterations) `div` period
                finalIterations = iterations + period * remainingLoops
            in (remainingLoops * (heightResultGrid - previousHeight)) + go finalIterations resultGrid pieces nextPieceIdx winds resultWindIdx DM.empty
        _ -> go (iterations + 1) resultGrid pieces nextPieceIdx winds resultWindIdx (DM.insert (pieceIdx, windIdx, heightMap) (iterations, heightResultGrid) seen)

main :: IO ()
main = do
    input <- DT.readFile ("17.input")

    case parseOnly (many1 parseDirection) input of
        Left err -> error err
        Right values -> do
            let
                initialGrid = DS.fromList [(0, x) | x <- [0..6]]

                result = go 0 initialGrid shapes 0 values 0 DM.empty

            print result