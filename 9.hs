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

data Direction = L | R | U | D
    deriving Show

readDirection = (L <$ char 'L') <|> (R <$ char 'R') <|> (U <$ char 'U') <|> (D <$ char 'D')

readMove = do
    direction <- readDirection
    char ' '
    steps <- decimal
    return (take steps (repeat direction))


applyStep L (x,y) = (x-1,y)
applyStep R (x,y) = (x+1,y)
applyStep U (x,y) = (x,y+1)
applyStep D (x,y) = (x,y-1)

moveTowards (hx,hy) (tx,ty) | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = (tx,ty)

moveTowards (hx,hy) (tx,ty) | hx == tx && ty < hy = (tx,ty+1)
moveTowards (hx,hy) (tx,ty) | hx == tx && ty > hy = (tx,ty-1)
moveTowards (hx,hy) (tx,ty) | tx < hx && hy == ty = (tx+1,ty)
moveTowards (hx,hy) (tx,ty) | tx > hx && hy == ty = (tx-1,ty)

moveTowards (hx,hy) (tx,ty) | tx > hx  && ty > hy = (tx-1,ty-1)
moveTowards (hx,hy) (tx,ty) | tx < hx  && ty > hy = (tx+1,ty-1)
moveTowards (hx,hy) (tx,ty) | tx > hx  && ty < hy = (tx-1,ty+1)
moveTowards (hx,hy) (tx,ty) | tx < hx  && ty < hy = (tx+1,ty+1)


-- step ((hx,hy), (tx,ty), visited) s = let
--     (newhx, newhy) = applyStep s (hx,hy)
--     (newtx, newty) = moveTowards (newhx, newhy) (tx,ty)
--     in ((newhx, newhy), (newtx, newty), visited++[(newtx, newty)])

step ((hx,hy), knots, visited) s = let
    (newhx, newhy) = applyStep s (hx,hy)
    (_, newknots) = foldl (\(h,knots) t -> let newposition = moveTowards h t in (newposition, knots++[newposition])) ((newhx, newhy), []) knots
    in ((newhx, newhy), newknots, visited++[last newknots])

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (concat <$> sepBy1 readMove (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            -- let result@(_,_,visited) = foldl step ((0,0), (0,0), []) values

            -- print result
            -- print (zip values visited)
            -- print (length (nub (visited)))

            let knots = take 9 (repeat (0,0))

            print knots

            let result@(_, _, visited) = foldl step ((0,0), knots, []) values

            print result
            print (length (nub (visited)))