module Main where

import           GameTree

import           Control.Arrow      (second)
import           Data.List          (foldl', intercalate)
import qualified Data.Map           as M
import           System.Environment

getSet :: Int -> HistoryView -> InformationSets Int -> (Int,InformationSets Int,Int)
getSet new hv set = case M.lookup hv set of
                        Nothing  -> (new, M.insert hv new set, new + 1)
                        Just id' -> (id', set, new)

indent :: (a, InformationSets b, Int) -> [String] -> [String]
indent (_,_,n) = (replicate n '\t' :)

indentWidth :: Int
indentWidth = 1

incIndent :: (a,InformationSets b,Int) -> (a,InformationSets b,Int)
incIndent (x,y,n) = (x,y,n+indentWidth)

decIndent :: (a,InformationSets b,Int) -> (a,InformationSets b,Int)
decIndent (x,y,n) = (x,y,n-indentWidth)

showTree :: GameTree -> String
showTree = concat . fst . go (1, M.empty, 0)
  where
    go :: (Int, InformationSets Int, Int)
       -> GameTree
       -> ([String], (Int, InformationSets Int, Int))
    go acc l@(Leaf _)  = (indent acc [show l, "\n"], acc)
    go acc (Nature ts) =
        second decIndent $ foldl' walkChildren (indent acc ["Nature\n"], incIndent acc)
                         $ map snd ts
    go acc@(i,s,ind) (Decide hv p ts)
        = let (id_,newS,newI) = getSet i hv s
              str = indent acc ["Decide ",show id_," ",show p,"\n"]
          in second decIndent $ foldl' walkChildren (str, incIndent (newI,newS,ind)) ts

    walkChildren :: ([String], (Int, InformationSets Int, Int))
                 -> GameTree
                 -> ([String], (Int, InformationSets Int, Int))
    walkChildren (have, acc) t = let (str, newAcc) = go acc t
                                 in (have ++ str, newAcc)

printConstraintMatrix :: [[Double]] -> IO ()
printConstraintMatrix = mapM_ printRow
  where
    printRow = putStrLn . intercalate "\t". map show

main :: IO ()
main = do
    [arg] <- getArgs
    putStr $ showTree $ mkTree $ read arg
