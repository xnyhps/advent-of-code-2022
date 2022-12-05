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

parseCrate = (Nothing <$ string "   ") <|> (do
    char '['
    name <- letter
    char ']'
    return (Just name))

parseStacks = sepBy1 (sepBy1 parseCrate (char ' ')) (char '\n')

parseNumbers = sepBy1 (do
    char ' '
    number <- decimal
    char ' '
    return number) (char ' ')


parseMoves = sepBy1 (do
    string "move "
    count <- decimal
    string " from "
    from <- decimal
    string " to "
    to <- decimal
    return (count, from, to)) (char '\n')

parseCrates = do
    stacks <- parseStacks
    char '\n'
    numbers <- parseNumbers
    string "\n\n"
    moves <- parseMoves
    return (DM.fromList (zip numbers (map catMaybes (transpose stacks))), moves)

-- evaluateMove stacks (count, from, to) = let
--     fromStack = DM.findWithDefault (error "uh oh") from stacks
--     movingCrates = take count fromStack
--     in DM.adjust (\x -> reverse movingCrates ++ x) to (DM.insert from (drop count fromStack) stacks)

evaluateMove stacks (count, from, to) = let
    fromStack = DM.findWithDefault (error "uh oh") from stacks
    movingCrates = take count fromStack
    in DM.adjust (\x -> movingCrates ++ x) to (DM.insert from (drop count fromStack) stacks)


main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly parseCrates input of
        Left err -> error err
        Right values@(stacks, moves) -> do
            print values

            print (evaluateMove stacks (head moves))

            let result = foldl evaluateMove stacks moves

            print (map (head . snd) $ DM.toList result)