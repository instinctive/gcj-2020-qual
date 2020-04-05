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
import Data.Maybe
import Data.Tuple
import Data.Traversable
import Debug.Trace hiding (trace)
import Text.Printf (printf)

import qualified Data.Array.Unboxed as UA
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S

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
    [n,k] <- getReadList :: IO [Int]
    case solve n k of
        Nothing -> putStrLn "IMPOSSIBLE"
        Just sq -> do
            putStrLn "POSSIBLE"
            for_ sq $ putStrLn . intercalate " " . map show

solve :: Int -> Int -> Maybe [[Int]]
solve n k
    | k < n || k > n*n = Nothing
    | otherwise = listToMaybe $ concatMap (latin n) (traces n k)

traces :: Int -> Int -> [[Int]]
traces n = go n 1 where
    go 0 _ 0 = [[]]
    go 0 _ k = error $ printf "leftover: %d" k
    go m j k = concat
        [ (x:) <$> go (m-1) x (k-x)
        | x <- [lo..hi]
        ]
      where
        lo = max j (k - n * (m-1))
        hi = div k m

latin :: Int -> [Int] -> [[[Int]]]
latin n trace =
    go coords known unknown
  where
    tr = let ary = UA.listArray (1,n) trace :: UA.UArray Int Int in (ary UA.!)
    cands = S.fromList [1..n]
    coords = [ (r,c) | r <- [1..n] , c <- [1..n] , r /= c ]
    known = M.fromList $ zipWith f [1..] trace where f i x = ((i,i),x)
    unknown = M.fromList $ f <$> coords where
        f (r,c) = ((r,c), S.delete (tr r) $ S.delete (tr c) cands)

    render m =
        [ [ m M.! (r,c) | c <- [1..n] ]
        | r <- [1..n] ]

    go [] k _ = [ render k ]
    go (q@(r,c):qq) k u = case maybe [] S.toList (M.lookup q u) of
        [] -> []
        xx -> concat
            [ go qq (M.insert q x k) (M.mapWithKey (f x) $ M.delete q u)
            | x <- xx ]
      where
        f x (r',c') s
            | r' == r || c' == c = S.delete x s
            | otherwise = s
