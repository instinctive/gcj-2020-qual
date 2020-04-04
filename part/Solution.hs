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

import qualified Data.Map.Strict as M

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
    tasks <- replicateM n getReadList
    putStrLn (solve tasks)

solve :: [[Int]] -> String
solve raw = go [] 0 0 cooked where
    rawmap = M.fromList (zip raw [0..])
    cooked = sort raw
    ordering = (rawmap M.!) <$> cooked

    go a _ _ [] = snd <$> sort (zip ordering $ reverse a)
    go a c j ([s,e]:xx)
        | s < c && s < j = "IMPOSSIBLE"
        | s < c = go ('J':a) c e xx
        | True  = go ('C':a) e j xx
