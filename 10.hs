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

data Instruction = Noop | Addx Integer
    deriving (Show)

readInstruction = (Noop <$ string "noop") <|> (do
    string "addx "
    deltax <- signed decimal
    return (Addx deltax))

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 readInstruction (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            -- let
            --     eval (x, cycl, output) Noop = (x, cycl + 1, if (cycl + 20) `mod` 40 == 0 then output ++ [x * cycl] else output)
            --     eval (x, cycl, output) (Addx deltax) = let
            --         newoutput = case (cycl + 20) `mod` 40 of
            --             0 -> output ++ [x * cycl]
            --             39 -> output ++ [x * (cycl + 1)]
            --             _ -> output
            --         in (x + deltax, cycl + 2, newoutput)

            --     result@(_,_,signals) = foldl eval (1, 1, []) values

            -- print result
            -- print (sum signals)

            let
                eval (x, cycl, output) Noop = (x, cycl + 1, if abs (x - (cycl `mod` 40)) <= 1 then output ++ ["#"] else output ++ ["."])

                eval (x, cycl, output) (Addx deltax) = let
                    bit1 = if abs (x - (cycl `mod` 40)) <= 1 then "#" else "."
                    bit2 = if abs (x - ((cycl + 1) `mod` 40)) <= 1 then "#" else "."
                    newoutput = output ++ [bit1, bit2]
                    in (x + deltax, cycl + 2, newoutput)

                result@(_,_,signals) = foldl eval (1, 0, []) values

            print result

            mapM_ putStrLn (chunksOf 40 (concat signals))