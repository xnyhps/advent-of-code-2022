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

parseMap = sepBy1 (many1 (True <$ char '#' <|> False <$ char '.')) (char '\n')

-- run :: Integer -> [((Integer, Integer), (Integer, Integer), (Integer, Integer))] -> DS.Set (Integer, Integer) -> DS.Set (Integer, Integer)
-- run 0 _ state = state
-- run count directions state = let
--     elves = DS.toList state
--     elvesWithProposedMoves = map (\(x,y) -> ((x,y), proposedMove (x,y))) elves

--     proposedMove (x,y) | all (`DS.notMember` state) [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0] = (x,y)
--     proposedMove (x,y) = proposedMove_ (x,y) directions

--     proposedMove_ (x,y) [] = (x,y)
--     proposedMove_ (x,y) (((dx1, dy1), (dx2, dy2), (dx3, dy3)):dis) | (x + dx1, y + dy1) `DS.notMember` state, (x + dx2, y + dy2) `DS.notMember` state, (x + dx3, y + dy3) `DS.notMember` state = (x + dx2, y + dy2)
--     proposedMove_ (x,y) (_:dirs) = proposedMove_ (x,y) dirs

--     isUniqueMove ((x,y),(px,py)) | length (filter (\(pos, prop) -> prop == (px,py)) elvesWithProposedMoves) == 1 = (px, py)
--     isUniqueMove ((x,y),_) = (x,y)
--     in run (count - 1) (take 4 (drop 1 (cycle directions))) (DS.fromList (map isUniqueMove elvesWithProposedMoves))

run :: Integer -> [((Integer, Integer), (Integer, Integer), (Integer, Integer))] -> DS.Set (Integer, Integer) -> Integer
run count directions state = let
    elves = DS.toList state
    elvesWithProposedMoves = map (\(x,y) -> ((x,y), proposedMove (x,y))) elves

    proposedMove (x,y) | all (`DS.notMember` state) [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0] = (x,y)
    proposedMove (x,y) = proposedMove_ (x,y) directions

    proposedMove_ (x,y) [] = (x,y)
    proposedMove_ (x,y) (((dx1, dy1), (dx2, dy2), (dx3, dy3)):dis) | (x + dx1, y + dy1) `DS.notMember` state, (x + dx2, y + dy2) `DS.notMember` state, (x + dx3, y + dy3) `DS.notMember` state = (x + dx2, y + dy2)
    proposedMove_ (x,y) (_:dirs) = proposedMove_ (x,y) dirs

    isUniqueMove ((x,y),(px,py)) | length (filter (\(pos, prop) -> prop == (px,py)) elvesWithProposedMoves) == 1 = (px, py)
    isUniqueMove ((x,y),_) = (x,y)

    newState = DS.fromList (map isUniqueMove elvesWithProposedMoves)
    in if state == newState then count else run (count + 1) (take 4 (drop 1 (cycle directions))) newState

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly parseMap input of
        Left err -> error err
        Right values -> do
            print values

            let
                grid = DS.fromList $ map fst $ filter snd $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values

                directions = 
                    [ ((-1, -1), (0, -1), (1, -1)) -- N
                    , ((-1, 1), (0, 1), (1, 1)) -- S
                    , ((-1, -1), (-1, 0), (-1, 1)) -- W
                    , ((1, -1), (1, 0), (1, 1)) -- E
                    ]

            print grid

            -- let result = run 10 directions grid

            -- forM_ [0..11] $ \y -> do
            --     forM_ [0..13] $ \x -> do
            --         putStr (if DS.member (x,y) result then "#" else ".")
            --     putStrLn ""

            -- let
            --     width = (maximum $ map fst $ DS.toList result) - (minimum $ map fst $ DS.toList result)
            --     height = (maximum $ map snd $ DS.toList result) - (minimum $ map snd $ DS.toList result)


            -- print ((width + 1) * (height + 1) - fromIntegral (DS.size result))

            let result = run 1 directions grid

            print result