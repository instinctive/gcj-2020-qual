-- vim: foldmethod=marker
-- Google CodeJam boilerplate {{{
{-# LANGUAGE LambdaCase    #-}
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
import System.Exit
import System.IO
import Text.Printf

import qualified Data.Map.Strict as M

getRead :: Read a => IO a
getRead = read <$> getLine

getReadList :: Read a => IO [a]
getReadList = map read . words <$> getLine

main :: IO ()
main = do
    [t,b] <- getReadList :: IO [Int]
    replicateM_ t $ docase b -- }}}

data Bits = OO | II | OI | IO deriving (Eq, Ord, Show)

docase :: Int -> IO ()
docase b = do
    grps <- for [0..ngrps-1] getGroup
    sync grps
  where
    ngrps = div (b+9) 10

    sync [(_,qq)] = do
        let answer = render qq
        putStrLn answer; hFlush stdout
        getLine >>= \case
            "Y" -> return ()
            "N" -> exitFailure

    sync grps = do
        bb <- concat <$> mapM update work
        sync ((0,bb) : more)
      where
        (work,more) = splitAt 5 grps

    update (offset,bb) = do
        same <- case findIndex (<OI) bb of
            Nothing -> babble >> return undefined
            Just i -> ask (offset+i) >>= \x -> return $ case (bb!!i,x) of
                (OO,0) -> id
                (II,1) -> id
                otherwise -> switch
        diff <- case findIndex (>II) bb of
            Nothing -> babble >> return undefined
            Just i -> ask (offset+i) >>= \x -> return $ case (bb!!i,x) of
                (OI,0) -> id
                (IO,1) -> id
                otherwise -> switch
        return $ adjust same diff <$> bb

    babble = ask 0 >>= \_ -> return ()

    switch OO = II
    switch II = OO
    switch OI = IO
    switch IO = OI

    adjust same diff = \case
        OO -> same OO
        II -> same II
        OI -> diff OI
        IO -> diff IO

    label qq = (findIndex (<OI) qq, findIndex (>II) qq)

    render = go [] [] where
        go ff bb []
            | even b    = reverse ff ++ bb
            | otherwise = reverse ff ++ tail bb
        go ff bb (q:qq) = go (f:ff) (b:bb) qq where
            (f,b) = case q of
                OO -> ('0','0')
                II -> ('1','1')
                OI -> ('0','1')
                IO -> ('1','0')

    getGroup :: Int -> IO (Int,[Bits])
    getGroup g = do
        qq <- for [lo..hi] getBits
        replicateM (4 - (hi - lo)) babble -- extra queries
        return (g*5,qq)
      where
        lo = g * 5
        hi = min (lo+4) (div (b-1) 2)

    getBits :: Int -> IO Bits
    getBits i = do
        x <- ask i
        y <- ask (b-i-1)
        return $ case (x,y) of
                (0,0) -> OO
                (1,1) -> II
                (0,1) -> OI
                (1,0) -> IO

    ask :: Int -> IO Int
    ask i = do
        print (i+1); hFlush stdout
        getRead
