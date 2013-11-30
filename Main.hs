module Main where

import           Algebra
import           GameTree

import           Data.Packed.Matrix
import           Numeric.LinearProgramming
import           System.Environment
import           Text.Printf

run :: Int -> IO ()
run k = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, yMap) = getSequenceMap seqs
        payoffMatrix = mkPayoffMatrix seqs
        matE = fromLists $ mkConstraintMatrix P1 xMap acts
        matF = fromLists $ mkConstraintMatrix P2 yMap acts
        xs = [1..rows payoffMatrix]
        zs = map (+length xs) [1..rows matF]

    let opt = Maximize $ replicate (length xs) 0 ++ [1] ++ replicate (length zs - 1) 0
    let c1 = fastConstrain (:==:) (matMult matE [1..rows payoffMatrix]) (1:repeat 0)

    let lhs = matMult (trans (negate payoffMatrix) <|> trans matF) (xs ++ zs)
    let c2  = fastConstrain (:<=:) lhs (repeat 0)
    let bounds = map Free zs
    let res = simplex opt (Sparse (c1 ++ c2)) bounds
    case res of
        Optimal (o,v) -> do putStrLn $ printf "%.3f" o
                            print v
        _ -> print res

  where
    fastConstrain :: ([(Double, Int)] -> Double -> Bound [(Double, Int)])
                  -> [[(Double, Int)]]
                  -> [Double]
                  -> [Bound [(Double, Int)]]
    fastConstrain = zipWith

    m <|> n = fromBlocks [[m, n]]

main :: IO ()
main = do
    [arg] <- getArgs
    run $ read arg
