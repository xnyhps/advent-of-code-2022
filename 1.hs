#!/usr/bin/env stack
{- stack
 runghc
 --package attoparsec
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text

import Control.Monad (void, forM_, when)
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List (transpose, permutations, sort)
import qualified Data.Map.Lazy as DM

import Debug.Trace

main :: IO ()
main = do
    input <- DT.readFile "1.input"

    case parseOnly (sepBy1 (sepBy1 decimal (char '\n')) (string "\n\n")) input of
        Left err -> error err
        Right values -> print (sum (Prelude.take 3 (reverse (sort (map sum values)))))