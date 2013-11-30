module Main where

import           Algebra
import           GameTree

import           Data.Matrix
import qualified Data.Vector               as V
import           Numeric.LinearProgramming
import           System.Environment
import           Text.Printf

run :: Int -> IO ()
run k = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, yMap) = getSequenceMap seqs
        payoffMatrix = mkPayoffMatrix seqs
        matE = fromLists $ mkConstraintMatrix P1 xMap acts
        matF = toMatrixD $ mkConstraintMatrix P2 yMap acts
        xNames = map (('x':).show) [1..nrows payoffMatrix]
        zNames = map (('z':).show.(+length xNames)) [1..nrows matF]
        xs = transpose $ toMatrixS [xNames]
        zs = transpose $ toMatrixS [zNames]

    let opt = Maximize $ replicate (length xNames) 0 ++ [1] ++ replicate (length zNames - 1) 0
    let c1 = fastConstrain (:==:) (matMult matE [1..nrows payoffMatrix]) (1:repeat 0)
    let lhs = transpose matF `multStd` zs
    let rhs = transpose payoffMatrix `multStd` xs
    let c2  = constrain (:<=:) (lhs - rhs) (zero (nrows lhs) 1)
    let c = c1 ++ c2
    let bounds = map (\z -> Free (read $ tail z)) zNames
    let res = simplex opt (Sparse c) bounds
    case res of
        Optimal (o,v) -> do putStrLn $ printf "%.3f" o
                            print v
        _ -> print res

  where
    constrain :: ([(Double, Int)] -> Double -> Bound [(Double,Int)])
              -> Matrix (Expr Double)
              -> Matrix (Expr Double)
              -> [Bound [(Double, Int)]]
    constrain op lhs rhs = map go [1..nrows lhs]
      where
        go i = let l = V.head (i `getRow` lhs)
                   r = V.head (i `getRow` rhs)
               in getVars l `op` (fromExpr r - fromExpr l)

    fastConstrain :: ([(Double, Int)] -> Double -> Bound [(Double, Int)])
                  -> [[(Double, Int)]]
                  -> [Double]
                  -> [Bound [(Double, Int)]]
    fastConstrain = zipWith

    matMult :: Matrix Double -> [Int] -> [[(Double, Int)]]
    matMult m vs = map go [1..nrows m]
      where
        go :: Int -> [(Double, Int)]
        go i = let row = i `getRow` m
               in zip (V.toList row) vs

main :: IO ()
main = do
    [arg] <- getArgs
    run $ read arg
