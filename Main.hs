module Main where

import           Algebra
import           GameTree

import           Data.List          (intercalate)
import           Data.Matrix
import qualified Data.Vector        as V
import           System.Environment

run :: Int -> IO ()
run k = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, yMap) = getSequenceMap seqs
        payoffMatrix = toMatrixD $ mkPayoffMatrix seqs
        matE = toMatrixD $ mkConstraintMatrix P1 xMap acts
        matF = toMatrixD $ mkConstraintMatrix P2 yMap acts
        vecE = transpose $ toMatrixD [1:replicate (nrows matE - 1) 0]
        vecF = transpose $ toMatrixD [1:replicate (nrows matF - 1) 0]
        xNames = map (('x':).show) [1..nrows payoffMatrix]
        zNames = map (('z':).show) [1..nrows matF]
        xs = transpose $ toMatrixS [xNames]
        zs = transpose $ toMatrixS [zNames]

    maximize $ multStd (transpose vecF) zs
    constrain "=" (multStd matE xs) vecE
    constrain "<=" (multStd (transpose matF) zs) (transpose payoffMatrix `multStd` xs)
    nonNeg xNames
    declare "sec" xNames
    declare "free" zNames
  where
    maximize e = putStrLn $ concat ["max: ", toStr $ e ! (1,1), ";"]
    constrain op lhs rhs = mapM_ go [1..nrows lhs]
      where
        go i = let l = i `getRow` lhs
                   r = i `getRow` rhs
               in putStrLn $ concat [toStr $ V.head l, " ", op, " ", toStr $ V.head r, ";"]
    nonNeg = mapM_ (\v -> putStrLn $ v ++ " >= 0;")
    declare t vs = putStrLn $ t ++ " " ++ intercalate ", " vs ++ ";"

main :: IO ()
main = do
    [arg] <- getArgs
    run $ read arg
