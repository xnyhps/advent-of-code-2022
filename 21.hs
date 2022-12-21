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

data Operation = Constant Integer | Add String String | Multiply String String | Subtract String String | Divide String String
    deriving (Show)

data Expr = C Integer | A Expr Expr | M Expr Expr | S Expr Expr | D Expr Expr | X

instance Show Expr where
    show (C c) = show c
    show (A l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
    show (M l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
    show (S l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
    show (D l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
    show X = "X"

parseLine = do
    name <- many1 letter
    string ": "
    expression <- choice [Constant <$> signed decimal, operator Add '+', operator Multiply '*', operator Subtract '-', operator Divide '/']
    return (name, expression)
    where
        operator f c = do
            l <- many1 letter
            char ' '
            char c
            char ' '
            r <- many1 letter
            return (f l r)

-- evaluateMonkey _ (Constant c) = c
-- evaluateMonkey monkeyMap (Add l r) = DM.findWithDefault (error "huh") l monkeyMap + DM.findWithDefault (error "huh") r monkeyMap
-- evaluateMonkey monkeyMap (Multiply l r) = DM.findWithDefault (error "huh") l monkeyMap * DM.findWithDefault (error "huh") r monkeyMap
-- evaluateMonkey monkeyMap (Subtract l r) = DM.findWithDefault (error "huh") l monkeyMap - DM.findWithDefault (error "huh") r monkeyMap
-- evaluateMonkey monkeyMap (Divide l r) = DM.findWithDefault (error "huh") l monkeyMap `div` DM.findWithDefault (error "huh") r monkeyMap

evaluateMonkey _ (Constant c) = C c
evaluateMonkey monkeyMap (Add l r) = DM.findWithDefault (error "huh") l monkeyMap `A` DM.findWithDefault (error "huh") r monkeyMap
evaluateMonkey monkeyMap (Multiply l r) = DM.findWithDefault (error "huh") l monkeyMap `M` DM.findWithDefault (error "huh") r monkeyMap
evaluateMonkey monkeyMap (Subtract l r) = DM.findWithDefault (error "huh") l monkeyMap `S` DM.findWithDefault (error "huh") r monkeyMap
evaluateMonkey monkeyMap (Divide l r) = DM.findWithDefault (error "huh") l monkeyMap `D` DM.findWithDefault (error "huh") r monkeyMap

minimizeMonkey (C c) = C c

minimizeMonkey (A l r) = let
    l_ = minimizeMonkey l
    r_ = minimizeMonkey r
    in case (l_, r_) of
        (C l__, C r__) -> C (l__ + r__)
        (l__, r__) -> A l__ r__


minimizeMonkey (M l r) = let
    l_ = minimizeMonkey l
    r_ = minimizeMonkey r
    in case (l_, r_) of
        (C l__, C r__) -> C (l__ * r__)
        (l__, r__) -> M l__ r__

minimizeMonkey (S l r) = let
    l_ = minimizeMonkey l
    r_ = minimizeMonkey r
    in case (l_, r_) of
        (C l__, C r__) -> C (l__ - r__)
        (l__, r__) -> S l__ r__

minimizeMonkey (D l r) = let
    l_ = minimizeMonkey l
    r_ = minimizeMonkey r
    in case (l_, r_) of
        (C l__, C r__) -> C (l__ `div` r__)
        (l__, r__) -> D l__ r__

minimizeMonkey X = X


main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            -- print values

            let monkeyMap = DM.fromList (map (\(name, operation) -> (name, if name == "humn" then X else evaluateMonkey monkeyMap operation)) values)

            -- print (DM.findWithDefault (error "huh") "root" monkeyMap)

            let
                Just (Add rootl rootr) = lookup "root" values
            
            print (rootl, rootr)

            print (minimizeMonkey (DM.findWithDefault (error "huh") rootr monkeyMap))
            print (minimizeMonkey (DM.findWithDefault (error "huh") rootl monkeyMap))