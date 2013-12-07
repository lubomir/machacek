module Main where

import           Algebra
import           GameTree

import           Control.Arrow         ((***))
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
        xs = map (('x':) . show) [0..rows payoffMatrix-1]
        zs = map (('z':) . show) [0..rows matF-1]

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

makeStrategy :: Int -> IO ()
makeStrategy k = do
    let (acts, seqs) = mkActions $ mkTree k
        (xMap, _yMap) = getSequenceMap seqs
    inp <- getContents
    let vars = parseVars inp
    let actsForP1 = filter ((`viewBelongsTo` P1) . fst) $ M.toList acts
    getStrategy xMap (fst vars) actsForP1

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lp", arg] -> makeLP $ read arg
        ["strategy", arg] -> makeStrategy $ read arg
        _ -> do putStrLn "Usage: machacek CMD SIZE"
                putStrLn "CMD: lp | strategy"
