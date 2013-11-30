{-# LANGUAGE FlexibleInstances #-}
module Algebra where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import           Data.Matrix
import           Data.Maybe         (fromMaybe)
import qualified Data.Vector        as V

-- | Given sizes of the matrix and values to put in it, construct
-- the actual matrix.
--
-- WARNING: while the indices in the third argument start from 0,
-- Data.Matrix.Matrix is indexed from 1.
buildMatrix :: Int -> Int -> IntMap (IntMap Double)-> Matrix Double
buildMatrix r c m = matrix r c go
  where
    go (x,y) = fromMaybe 0 (I.lookup (x-1) m >>= I.lookup (y-1))

-- | Given a matrix and a column vector of variable numbers, produce as column
-- vector produced by matrix multiplication. Tuples are (coefficient, variable).
--
matMult :: Matrix Double -> [Int] -> [[(Double, Int)]]
matMult m vs = map go [1..nrows m]
  where
    go :: Int -> [(Double, Int)]
    go i = let row = i `getRow` m
           in zip (V.toList row) vs
