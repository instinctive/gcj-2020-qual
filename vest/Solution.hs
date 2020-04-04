-- vim: foldmethod=marker
-- Google CodeJam boilerplate {{{
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List
import Data.Tuple
import Data.Traversable
import Debug.Trace
import Text.Printf (printf)

getRead :: Read a => IO a
getRead = read <$> getLine

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

main :: IO ()
main = do
    t <- getRead :: IO Int
    for_ [1..t] $ \c -> do
        printf "Case #%d: " c
        docase -- }}}

docase :: IO ()
docase = do
    n <- getRead :: IO Int
    sqr <- replicateM n getReadList :: IO [[Int]]
    let (k,r,c) = solve sqr
    printf "%d %d %d\n" k r c

solve :: [[Int]] -> (Int,Int,Int)
solve sqr = (k,r,c) where
    k = sum $ zipWith (!!) sqr [0..]
    r = repeats sqr
    c = repeats (transpose sqr)
    repeats = length . filter not . map (latin . sort)
    latin ss = and $ zipWith (==) (succ <$> ss) (tail ss)
