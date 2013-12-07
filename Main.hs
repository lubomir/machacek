module Main where

import           Algebra
import           GameTree
import           LPSolve

import           Control.Arrow         ((***), first)
import           Control.Monad         (when)
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as I
import           Data.List             (partition, intercalate)
import qualified Data.Map              as M
import           Data.Maybe (fromJust)
import qualified Data.ListTrie.Patricia.Map     as T
import           Data.ListTrie.Patricia.Map.Ord (TrieMap)
import           Numeric.LinearAlgebra (fromBlocks, fromLists, rows, trans)
import           System.Environment
import           Text.Printf

makeLP :: Int -> IO (Double, [(String, Double)])
makeLP k = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, yMap) = getSequenceMap seqs
        payoffMatrix = mkPayoffMatrix seqs
        matE = fromLists $ mkConstraintMatrix P1 xMap acts
        matF = fromLists $ mkConstraintMatrix P2 yMap acts
        xs = map (('x':) . show) [0..rows payoffMatrix-1]
        zs = map (('z':) . show) [0..rows matF-1]
        lhs = matMult (trans (negate payoffMatrix) <|> trans matF) (xs ++ zs)

    lpSolve $ do
        maximize $ head zs
        constrain "=" (matMult matE xs) (1:repeat 0)
        constrain "<=" lhs (repeat 0)
        setFree zs
  where
    m <|> n = fromBlocks [[m, n]]

parseVars :: [(String, Double)] -> (IntMap Double, IntMap Double)
parseVars = (toMap *** toMap) . partition ((=='x') . head . fst)
  where
    toMap = I.fromList . map (first (read . tail))

getStrategy :: TrieMap Act Int  -- ^Sequence numbering
            -> IntMap Double    -- ^Variables from linear program
            -> [(HistoryView, (Sequence, [Act]))]
                                -- ^List of information sets, their sequences
                                --  and possible actions
            -> IO ()
getStrategy m vars = mapM_ toDecision
  where
    var = fromJust . flip I.lookup vars . fromJust . flip T.lookup m
    toDecision (hist, (sq, actions)) =
        when (parent > 0) $ do
            printHistory hist
            mapM_ go $ zip [0..] $ map ((/parent) . var . (: sq)) actions
      where
        parent = var sq

        go :: (Int, Double) -> IO ()
        go (_,0) = return ()
        go (n,p) = putStrLn $ printf "  %s with prob. %.3f" a p
          where
            a = case last hist of
                    Heard _     -> if n == 0 then "Trust" else "Do not trust"
                    Performed e -> "Say " ++ show (rolled e + n)

-- |Print history view in user friendly way.
--
printHistory :: HistoryView -> IO ()
printHistory h = putStrLn $ "Situation: " ++ intercalate ", " (map go h)
  where
    go (Heard n) = "heard " ++ show n
    go (Performed e) = case said e of
        0 -> "rolled " ++ show (rolled e)
        i -> "rolled " ++ show (rolled e) ++ " and said " ++ show i

makeStrategy :: Int -> [(String, Double)] -> IO ()
makeStrategy k inp = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, _yMap) = getSequenceMap seqs
    let vars = parseVars inp
    let actsForP1 = filter ((`viewBelongsTo` P1) . fst) $ M.toList acts
    getStrategy xMap (fst vars) actsForP1

main :: IO ()
main = do
    [arg] <- getArgs
    (opt, vars) <- makeLP (read arg)
    putStrLn $ printf "Value of game is %.3f\n" opt
    makeStrategy (read arg) vars
