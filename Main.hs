module Main where

import           GameTree
import           LPSolve

import           Control.Arrow                  (first)
import           Control.Monad                  (when)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as I
import           Data.List                      (intercalate, partition)
import qualified Data.ListTrie.Patricia.Map     as T
import           Data.ListTrie.Patricia.Map.Ord (TrieMap)
import           Data.Maybe                     (fromJust, fromMaybe)
import           System.Environment
import           Text.Printf
import           Math.LinearAlgebra.Sparse.Matrix

-- | Given a matrix and a column vector of variables, create a column vector
-- produced by matrix multiplication. Tuples are (coefficient, variable).
--
matMult :: SparseMatrix a -> [b] -> [[(a, b)]]
matMult !m vs' = map (reverse . go [] 1 vs' . I.assocs) $ I.elems $ mx m
  where
    go acc _ [] _ = acc
    go acc _ _ [] = acc
    go acc n (v:vs) y@((i,c):rs)
      | n == i    = go ((c,v):acc) (n+1) vs rs
      | otherwise = go acc         (n+1) vs y

-- |Only take variables that start with given character.
--
filterVars :: Char -> [(String, Double)] -> IntMap Double
filterVars v = I.fromList . map (first (read . tail))
                          . filter ((==v) . head . fst)

getStrategy :: TrieMap Act Int  -- ^Sequence numbering
            -> IntMap Double    -- ^Variables from linear program
            -> [(HistoryView, (Sequence, [Act]))]
                                -- ^List of information sets, their sequences
                                --  and possible actions
            -> IO ()
getStrategy m vars = mapM_ toDecision
  where
    var = fromMaybe 0 . flip I.lookup vars . flip (-) 1 . fromJust . flip T.lookup m
    toDecision (hist, (sq, actions)) =
        when (parent > 0) $ do
            printHistory hist
            mapM_ go $ zip [0..] $ map ((/parent) . var . (: sq)) actions
      where
        parent = var sq

        go :: (Int, Double) -> IO ()
        go (_,0) = return ()
        go (n,1) = putStrLn $ "  " ++ a n
        go (n,p) = putStrLn $ printf "  %s with prob. %.3f" (a n) p

        a n  = case last hist of
                Heard _     -> if n == 0 then "Trust" else "Do not trust"
                Performed e -> "Say " ++ show (firstOption e + n)
          where
            firstOption e = max (rolled e) (lastHeard hist + 1)

-- |Print history view in user friendly way.
--
printHistory :: HistoryView -> IO ()
printHistory h = putStrLn $ "Situation: " ++ intercalate ", " (map go h)
  where
    go (Heard n) = "heard " ++ show n
    go (Performed e) = case said e of
        0 -> "rolled " ++ show (rolled e)
        i -> "rolled " ++ show (rolled e) ++ " and said " ++ show i

run :: Int -> IO ()
run k = do
    let (acts, payoffMatrix, xMap, yMap) = mkActions $ mkTree k
        matE = mkConstraintMatrix P1 xMap acts
        matF = mkConstraintMatrix P2 yMap acts
        xs = map (('x':) . show) [0..height payoffMatrix-1]
        zs = map (('z':) . show) [0..height matF-1]
        ys = map (('y':) . show) [0..width matF-1]
        lhs = (trans (negate payoffMatrix) <|> trans matF) `matMult` (xs ++ zs)

    (opt, vars) <- lpSolve $ do
        maximize $ head zs
        constrain "=" (matE `matMult` xs) (1:repeat 0)
        constrain "<=" lhs (repeat 0)
        setFree zs
    putStrLn $ printf "Value of game is %.3f\n" opt

    let xVars = filterVars 'x' vars
        (actsForP1,actsForP2) = partition ((`viewBelongsTo` P1) . fst) $ T.toList acts
        coeff = sparseMx [map snd $ I.toAscList xVars] `mul` payoffMatrix

    putStrLn "Strategy for player 1"
    getStrategy xMap xVars actsForP1

    (_,res) <- lpSolve $ do
        minimize $ head $ coeff `matMult` ys
        constrain "=" (matF `matMult` ys) (1:repeat 0)

    putStrLn "\nStrategy for player 2"
    getStrategy yMap (filterVars 'y' res) actsForP2
  where
    m <|> n = hconcat [m, n]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> run $ read arg
        _     -> putStrLn "Usage: machacek K"
