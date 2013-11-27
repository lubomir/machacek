{-# LANGUAGE FlexibleInstances #-}
module Algebra where

import           Data.List   (foldl')
import qualified Data.Map    as M
import           Data.Matrix
import           Text.Printf

data Expr a = Expr a (M.Map String a)
    deriving (Eq, Ord)

instance Num a => Num (Expr a) where
    (Expr c1 vs1) + (Expr c2 vs2) = Expr (c1+c2) $ M.unionWith (+) vs1 vs2
    (Expr c1 vs1) * (Expr c2 vs2)
        | M.null vs1 = Expr (c1*c2) $ M.map (*c1) vs2
        | M.null vs2 = Expr (c1*c2) $ M.map (*c2) vs1
        | otherwise  = error "Non-linear function after multiplication"
    abs (Expr c vs) = Expr (abs c) $ M.map abs vs
    signum (Expr c vs) = Expr (signum c) $ M.map signum vs
    fromInteger n = Expr (fromInteger n) M.empty

toStr :: Expr Double -> String
toStr (Expr c vars) = addC c $ go "" $ M.toList vars
  where
    p = printf "%.5f"
    addC 0 "" = "0"
    addC 0 (' ':'+':' ':s)  = s
    addC 0 (' ':'-':' ':s)  = '-':s
    addC n "" = p n
    addC n s  = p n ++ " + " ++ s
    go acc [] = acc
    go acc ((v,n):vs) = go (acc ++ h v n) vs
    h v n
      | n == 0    = ""
      | n == 1    = ' ':'+':' ':v
      | n == -1   = ' ':'-':' ':v
      | n < 0     = concat [" - ", p (-n), "*", v]
      | otherwise = concat [" + ", p n, "*", v]

instance Show (Expr Double) where
    show = toStr

toMatrixD :: [[Double]] -> Matrix (Expr Double)
toMatrixD = fromLists . map (map (`Expr` M.empty))

toMatrixS :: [[String]] -> Matrix (Expr Double)
toMatrixS = fromLists . map (map (\v -> Expr 0 $ M.singleton v 1))

buildMatrix :: Int -> Int -> [(Int, Int, Double)] -> Matrix (Expr Double)
buildMatrix r c = foldl' insert (zero r c)
  where
    insert m (x,y,p) = let oldVal = getElem x y m
                       in setElem (oldVal + Expr p M.empty) (x,y) m
