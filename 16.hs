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

parseIdentifier = do
    a <- letter
    b <- letter
    return (Identifier a b)

data Identifier = Identifier Char Char
    deriving (Ord, Eq)

instance Show Identifier where
    show (Identifier a b) = [a,b]

data State = State
    { me :: Maybe (Identifier, Int)
    , elephant :: Maybe (Identifier, Int)
    , roundCount :: Int
    , openValves :: DS.Set Identifier
    } deriving (Show)

parseLine = do
    string "Valve "
    name <- parseIdentifier
    string " has flow rate="
    flowRate <- decimal
    string "; tunnels lead to valves " <|> string "; tunnel leads to valve "
    tunnels <- sepBy parseIdentifier (string ", ")
    return (name, flowRate, tunnels)

minuteScore :: DM.Map Identifier Integer -> DS.Set Identifier -> Integer
minuteScore flowRates openValves = sum (map (\valve -> DM.findWithDefault (error "huh") valve flowRates) (DS.toList openValves))

eval :: DM.Map Identifier (DM.Map Identifier Int) -> DM.Map Identifier Integer -> State -> [Integer]
eval graph flowRates (State { roundCount = 26, .. }) = []
eval graph flowRates (State { .. }) = let
    newOpenValves = (case elephant of
        Just (elephantOpening, 0) -> (DS.insert elephantOpening)
        _ -> id) ((case me of
        Just (myOpening, 0) -> (DS.insert myOpening)
        _ -> id) openValves)
    newRoundCount = roundCount + 1

    myOptions = case me of
        Just (myOpening, 0) -> Nothing : map Just (filter (\(identifier,_) -> identifier `notElem` newOpenValves) (DM.toList (DM.findWithDefault (error "huh") myOpening graph)))
        Just (myOpening, myCountdown) -> [Just (myOpening, myCountdown - 1)]
        Nothing -> [Nothing]

    elephantOptions = case elephant of
        Just (elephantOpening, 0) -> Nothing : map Just (filter (\(identifier,_) -> identifier `notElem` newOpenValves) (DM.toList (DM.findWithDefault (error (show elephantOpening)) elephantOpening graph)))
        Just (elephantOpening, elephantCountdown) -> [Just (elephantOpening, elephantCountdown - 1)]
        Nothing -> [Nothing]

    subOptions = map (eval graph flowRates) [State { me = me, elephant = elephant, roundCount = newRoundCount, openValves = newOpenValves }
                                            | me <- myOptions
                                            , elephant <- elephantOptions
                                            ]

    roundScore = minuteScore flowRates newOpenValves
    in (roundScore : (case subOptions of
        [] -> []
        _ -> maximumBy (comparing sum) subOptions))

dijkstra :: Identifier -> DM.Map Identifier [Identifier] -> DM.Map Identifier Int
dijkstra start edges = loop [start] DS.empty (DM.singleton start 0)
    where
        loop :: [Identifier] -> DS.Set Identifier -> DM.Map Identifier Int -> DM.Map Identifier Int
        loop [] _ weights = weights
        loop (point:queue) seen weights | point `DS.member` seen = loop queue seen weights
        loop (point:queue) seen weights = let
                thisWeight = DM.findWithDefault (error "huh") point weights
                outgoing = DM.findWithDefault (error "huh") point edges

                f Nothing = Just (thisWeight + 1)
                f (Just x) | x > thisWeight + 1 = Just (thisWeight + 1)
                f (Just x) | otherwise = Just x

                newWeights = foldr (\neighbour weights -> DM.alter f neighbour weights) weights outgoing

                newQueue = sortBy (comparing (\k -> DM.findWithDefault maxBound k weights)) (filter (`DS.notMember` seen) (queue ++ outgoing))
            in loop newQueue (DS.insert point seen) newWeights

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            let
                flowRates = DM.fromList (map (\(a,b,c) -> (a,b)) values)
                tunnels = DM.fromList (map (\(a,b,c) -> (a,c)) values)
                graph = DM.map (\weights -> DM.filterWithKey (\identifier _ -> DM.findWithDefault (error "huh") identifier flowRates > 0) weights)
                            (DM.filterWithKey (\identifier _ -> DM.findWithDefault (error "huh") identifier flowRates > 0 || identifier == (Identifier 'A' 'A'))
                                (DM.fromList (map (\(a,b,c) -> (a, dijkstra a tunnels)) values)))

                result = eval graph flowRates (State { roundCount = 0, openValves = DS.empty, me = Just (Identifier 'A' 'A', 0), elephant = Just (Identifier 'A' 'A', 0) })

            print graph

            mapM_ print result
            print (sum result)
