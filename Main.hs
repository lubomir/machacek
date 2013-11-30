module Main where

import           Algebra
import           GameTree

import           Data.List          (intercalate)
import           Data.Packed.Matrix
import           System.Environment
import           Text.Printf

run :: Int -> IO ()
run k = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, yMap) = getSequenceMap seqs
        payoffMatrix = mkPayoffMatrix seqs
        matE = fromLists $ mkConstraintMatrix P1 xMap acts
        matF = fromLists $ mkConstraintMatrix P2 yMap acts
        xs = map (('x':) . show) [1..rows payoffMatrix]
        zs = map (('z':) . show) [1..rows matF]

    putStrLn $ "max: " ++ head zs ++ ";"
    fastConstrain "=" (matMult matE xs) (1:repeat 0)

    let lhs = matMult (trans (mapMatrix negate payoffMatrix) <|> trans matF) (xs ++ zs)
    fastConstrain "<=" lhs (repeat 0)
    setBounds zs

  where
    fastConstrain op lhs rhs = mapM_ go $ zip lhs rhs
      where
        go (l, r) = putStrLn $ concatMap f l ++ op ++ show r ++ ";"
        f (n, v)
          | n == 0 = ""
          | n > 0  = '+':p n ++ v
          | n < 0  = p n ++ v

        p = printf "%.5f"

    setBounds vs = putStrLn $ "free " ++ intercalate ", " vs ++ ";"

    m <|> n = fromBlocks [[m, n]]

main :: IO ()
main = do
    [arg] <- getArgs
    run $ read arg
