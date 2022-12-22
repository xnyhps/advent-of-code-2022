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
import qualified Data.Map.Lazy as DM
import qualified Data.Set as DS
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Traversable (for)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

data Instruction = Move Integer | RotateLeft | RotateRight
    deriving (Show)

data Tile = Empty | Wall | Floor
    deriving (Show, Eq)

parseInstruction = choice [Move <$> decimal, RotateLeft <$ char 'L', RotateRight <$ char 'R']

parseLine = many1 (Empty <$ char ' ' <|> Wall <$ char '#' <|> Floor <$ char '.')

parseMap = do
    grid <- sepBy1 parseLine (char '\n')
    string "\n\n"
    instructions <- many1 parseInstruction
    return (grid, instructions)

move :: [Instruction] -> (Integer, Integer) -> Integer -> DM.Map (Integer, Integer) Tile -> Integer -> Integer -> ((Integer, Integer), Integer)
move [] point angle _ _ _ = (point, angle)
move (RotateLeft:instructions) point angle grid width height = move instructions point ((angle - 1) `mod` 4) grid width height
move (RotateRight:instructions) point angle grid width height = move instructions point ((angle + 1) `mod` 4) grid width height
move (Move distance:instructions) point angle grid width height = let (newAngle, newPosition) = takeStep distance (delta angle) point angle in move instructions newPosition newAngle grid width height
    where
        delta angle = case angle of
            0 -> (1,0)
            1 -> (0,1)
            2 -> (-1, 0)
            3 -> (0, -1)

        -- takeStep 0 _ (x,y) = (x,y)
        -- takeStep _ (dx, dy) (x,y) | Just Wall <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = (x,y)
        -- takeStep n (dx, dy) (x,y) | Just Floor <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = takeStep (n - 1) (dx, dy) ((x + dx) `mod` width, (y + dy) `mod` height)
        -- takeStep n (dx, dy) (x,y) | Just Empty <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = case teleport (dx, dy) ((x + dx) `mod` width, (y + dy) `mod` height) of
        --     Just (newx, newy) -> takeStep (n - 1) (dx, dy) (newx, newy)
        --     Nothing -> (x,y)
        -- takeStep n (dx, dy) (x,y) | Nothing <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = case teleport (dx, dy) ((x + dx) `mod` width, (y + dy) `mod` height) of
        --     Just (newx, newy) -> takeStep (n - 1) (dx, dy) (newx, newy)
        --     Nothing -> (x,y)

        -- teleport (dx, dy) (x,y) | Just Wall <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = Nothing
        -- teleport (dx, dy) (x,y) | Just Floor <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = Just ((x + dx) `mod` width, (y + dy) `mod` height)
        -- teleport (dx, dy) (x,y) | Just Empty <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = teleport (dx, dy) ((x + dx) `mod` width, (y + dy) `mod` height)
        -- teleport (dx, dy) (x,y) | Nothing <- DM.lookup ((x + dx) `mod` width, (y + dy) `mod` height) grid = teleport (dx, dy) ((x + dx) `mod` width, (y + dy) `mod` height)

        takeStep 0 _ (x,y) angle = (angle, (x,y))
        takeStep _ (dx, dy) (x,y) angle | Just Wall <- DM.lookup ((x + dx), (y + dy)) grid = (angle, (x,y))
        takeStep n (dx, dy) (x,y) angle | Just Floor <- DM.lookup ((x + dx), (y + dy)) grid = takeStep (n - 1) (dx, dy) ((x + dx), (y + dy)) angle
        takeStep n (dx, dy) (x,y) angle | Nothing <- DM.lookup ((x + dx), (y + dy)) grid = let (otherEnd, dangle) = teleport ((x + dx), (y + dy)) in case DM.lookup otherEnd grid of
            Just Wall -> (angle, (x, y))
            Just Floor -> let newAngle = ((angle + dangle) `mod` 4) in takeStep (n - 1) (delta newAngle) otherEnd newAngle
            _ -> error ("Should not happen: " ++ show otherEnd)

        teleport (x,y) | x == 49 && 0 <= y && y < 50 = ((0, 149 - y), 2)
        teleport (x,y) | x == -1 && 100 <= y && y < 150 = ((50, 149 - y), 2)

        teleport (x,y) | x == -1 && 150 <= y && y < 200 = ((y - 100, 0), 3)
        teleport (x,y) | y == -1 && 50 <= x && x < 100 = ((0, 100 + x), 1)

        teleport (x,y) | x == 49 && 50 <= y && y < 100 = ((y - 50, 100), 3)
        teleport (x,y) | y == 99 && 0 <= x && x < 50 = ((50, 50 + x), 1)

        teleport (x,y) | y == 200 && 0 <= x && x < 50 = ((x + 100, 0), 0)
        teleport (x,y) | y == -1 && 100 <= x && x < 150 = ((x - 100, 199), 0)

        teleport (x,y) | x == 50 && 150 <= y && y < 200 = ((y - 100, 149), 3)
        teleport (x,y) | y == 150 && 50 <= x && x < 100 = ((49, x + 100), 1)

        teleport (x,y) | x == 100 && 100 <= y && y < 150 = ((149, 149 - y), 2)
        teleport (x,y) | x == 150 && 0 <= y && y < 50 = ((99, 149 - y), 2)

        teleport (x,y) | x == 100 && 50 <= y && y < 100 = ((y + 50, 49), 3)
        teleport (x,y) | y == 50 && 100 <= x && x < 150 = ((99, x - 50), 1)

        teleport (x,y) = error (show (x,y))

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly parseMap input of
        Left err -> error err
        Right (m, instructions) -> do
            -- print (m, instructions)

            let
                width = fromIntegral (length (head m))
                height = fromIntegral (length m)

                grid = DM.filter (\x -> x /= Empty) $ DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) m
                startingPoint = (\x -> (x,0)) $ head $ filter (\x -> DM.lookup (x,0) grid == Just Floor) [1..width]

                result@((column, row), angle) = move instructions startingPoint 0 grid width height

            -- print grid
            print startingPoint
            print result
            print ((row + 1) * 1000 + (column + 1) * 4 + angle)