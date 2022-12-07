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
import Data.List (transpose, permutations, intersect, nub, tails, sortBy)
import qualified Data.Map.Lazy as DM
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

data CDArgument = Root | Parent | Name DT.Text
    deriving (Show)

data LSResult = Directory DT.Text | File Integer DT.Text
    deriving (Show)

data Command = CD CDArgument | LS [LSResult]
    deriving (Show)

parseDirectory = do
    string "dir "
    name <- many1 letter
    return (Directory (DT.pack name))

parseFile = do
    size <- decimal
    char ' '
    name <- many1 (letter <|> char '.')
    return (File size (DT.pack name))

parseLs = do
    string "ls\n"
    LS <$> sepBy (parseDirectory <|> parseFile) (char '\n')

parseCd = do
    string "cd "
    argument <- Parent <$ string ".." <|> Root <$ string "/" <|> Name . DT.pack <$> many1 letter
    return (CD argument)

parseCommandAndOutput = do
    string "$ "
    parseCd <|> parseLs

parseCommands = sepBy1 parseCommandAndOutput (char '\n')

g _ (Directory _) sizes = sizes
g path (File size name) sizes = DM.insert (name:path) size sizes

f (path, sizes) (CD Root) = ([], sizes)
f ((x:xs), sizes) (CD Parent) = (xs, sizes)
f (xs, sizes) (CD (Name x)) = (x:xs, sizes)
f (path, sizes) (LS files) = (path, foldr (g path) sizes files)

h sizes ((file:directory), size) = let
        alterFunction Nothing = Just size
        alterFunction (Just x) = Just (x + size)
    in foldr (\a b -> DM.alter alterFunction a b) sizes (tails directory)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly parseCommands input of
        Left err -> error err
        Right values -> do
            print values

            let
                (_, filesWithSizes) = foldl f ([], DM.empty) values
                directoriesWithSizes = foldl h DM.empty (DM.toList filesWithSizes)
                matchingDirectories = sum $ map snd $ filter (\(a, b) -> b < 100000) (DM.toList directoriesWithSizes)

            -- print filesWithSizes
            -- print directoriesWithSizes
            -- print matchingDirectories

            let
                sizeUsed = DM.findWithDefault (error "Huh") [] directoriesWithSizes
                sizeAvailable = 70000000 - sizeUsed
                sizeNeeded = 30000000 - sizeAvailable
                candidates = dropWhile (\(_, size) -> size < sizeNeeded) $ sortBy (comparing snd) (DM.toList directoriesWithSizes)

            print sizeUsed
            print sizeAvailable
            print sizeNeeded
            print (head candidates)