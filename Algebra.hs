module Algebra where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import           Data.Maybe         (fromMaybe)
import           Data.Packed.Matrix
import qualified Data.Packed.Vector as V

-- | Given sizes of the matrix and values to put in it, construct
-- the actual matrix.
mkMatrix :: (Num a, Element a) => Int -> Int -> IntMap (IntMap a) -> Matrix a
mkMatrix r c m = buildMatrix r c go
  where
    go (x,y) = fromMaybe 0 (I.lookup x m >>= I.lookup y)

-- | Given a matrix and a column vector of variables, create a column vector
-- produced by matrix multiplication. Tuples are (coefficient, variable).
--
matMult :: (Element a) => Matrix a -> [b] -> [[(a, b)]]
matMult m vs = map go $ toRows m
  where
    go r = zip (V.toList r) vs
