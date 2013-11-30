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
buildMatrix :: Num a => Int -> Int -> IntMap (IntMap a) -> Matrix a
buildMatrix r c m = matrix r c go
  where
    go (x,y) = fromMaybe 0 (I.lookup (x-1) m >>= I.lookup (y-1))

-- | Given a matrix and a column vector of variables, create a column vector
-- produced by matrix multiplication. Tuples are (coefficient, variable).
--
matMult :: Matrix a -> [b] -> [[(a, b)]]
matMult m vs = map go [1..nrows m]
  where
    go i = zip (V.toList $ i `getRow` m) vs
