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

data Height = Start | End | Height Integer
    deriving (Show, Eq)

parseHeight = (Start <$ char 'S') <|> (End <$ char 'E') <|> (makeHeight <$> letter)
    where
        makeHeight c = Height $ fromIntegral (ord c - ord 'a')

getHeight Start = 0
getHeight End = 25
getHeight (Height x) = x

updateWeight height weight grid weights (x,y) | Nothing <- DM.lookup (x,y) grid = weights
updateWeight height weight grid weights (x,y)
    | Just targetHeight <- getHeight <$> DM.lookup (x,y) grid
    , targetHeight >= height - 1
    = let
        f Nothing = Just (weight + 1)
        f (Just w) | w > weight + 1 = Just (weight + 1)
        f x = x
    in DM.alter f (x,y) weights
updateWeight height weight grid weights (x,y) = weights

makeQueue seen weights = filter (\x -> x `notElem` seen) (map fst (sortBy (comparing snd) (DM.toList weights)))

dijkstra _ [] weights _ = error "No path found"
dijkstra grid ((x,y):queue) weights seen = let
    height = getHeight $ DM.findWithDefault (error "huh") (x,y) grid
    weight = DM.findWithDefault (error "huh") (x,y) weights

    newWeights = foldl (updateWeight height weight grid) weights [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx == 0 || dy == 0, (dx, dy) /= (0,0)]
    newSeen = DS.insert (x,y) seen

    newQueue = makeQueue newSeen newWeights

    in if height == 0 then weight else dijkstra grid newQueue newWeights newSeen

findRoute (sx, sy) grid = let
    weights = DM.fromList [((sx,sy), 0)]
    queue = [(sx, sy)]
    seen = DS.empty
    
    in dijkstra grid queue weights seen

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 (many1 parseHeight) (char '\n')) input of
        Left err -> error err
        Right values -> do
            let
                grid = DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values

                start = map (elemIndex Start) values
                sx = fromIntegral (head (catMaybes start))
                sy = fromIntegral (fromJust (findIndex isJust start))

                end = map (elemIndex End) values
                ex = fromIntegral (head (catMaybes end))
                ey = fromIntegral (fromJust (findIndex isJust end))

                -- routeFromBs = map (\y -> (findRoute (1, y) (ex, ey) grid, y)) [0..length values]


            print (findRoute (ex, ey) grid)