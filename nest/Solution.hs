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
docase = getLine >>= putStrLn . solve

solve :: String -> String
solve = go 0 where
    go n [] = replicate n ')'
    go n (c:cc) = buf ++ [c] ++ go m cc where
        m = digitToInt c
        buf | n > m     = replicate (n-m) ')'
            | otherwise = replicate (m-n) '('
