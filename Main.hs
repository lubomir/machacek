module Main where

import           Algebra
import           GameTree

import           Control.Arrow
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as I
import           Data.List             (partition)
import           Data.List             (intercalate)
import           Numeric.LinearAlgebra (fromBlocks, fromLists, rows, trans)
import           System.Environment
import           Text.Printf

constrain :: String -> [[(Double, String)]] -> [Int] -> IO ()
constrain op lhs rhs = mapM_ go $ zip lhs rhs
  where
    go (l, r) = putStrLn $ concatMap f l ++ op ++ show r ++ ";"
    f (n, v)
      | n == 0 = ""
      | n > 0  = '+':p n ++ v
      | n < 0  = p n ++ v
    f (_, _) = error "How is this even possible?"

    p = printf "%.5f"

makeLP :: Int -> IO ()
makeLP k = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, yMap) = getSequenceMap seqs
        payoffMatrix = mkPayoffMatrix seqs
        matE = fromLists $ mkConstraintMatrix P1 xMap acts
        matF = fromLists $ mkConstraintMatrix P2 yMap acts
        xs = map (('x':) . show) [1..rows payoffMatrix]
        zs = map (('z':) . show) [1..rows matF]

    maximize $ head zs
    constrain "=" (matMult matE xs) (1:repeat 0)

    let lhs = matMult (trans (negate payoffMatrix) <|> trans matF) (xs ++ zs)
    constrain "<=" lhs (repeat 0)
    setBounds zs
  where
    maximize = putStrLn . printf "max: %s;"
    setBounds vs = putStrLn $ "free " ++ intercalate ", " vs ++ ";"
    m <|> n = fromBlocks [[m, n]]

parseVars :: String -> (IntMap Double, IntMap Double)
parseVars = (toMap *** toMap) . partition ((=='x') . head) . filter hasVar . lines
  where
    hasVar ('x':_) = True
    hasVar ('z':_) = True
    hasVar _ = False

    toPair [_:idx,val] = (read idx, read val)
    toPair _ = error "Parse error"

    toMap :: [String] -> IntMap Double
    toMap = I.fromList . map (toPair . words)

makeStrategy :: Int -> IO ()
makeStrategy k = do
    let (acts, seqs) = mkActions $ mkTree k
    inp <- getContents
    let vars = parseVars inp
    print vars

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lp", arg] -> makeLP $ read arg
        ["strategy", arg] -> makeStrategy $ read arg
        _ -> do putStrLn "Usage: machacek CMD SIZE"
                putStrLn "CMD: lp | strategy"
