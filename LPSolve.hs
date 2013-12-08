module LPSolve ( LinearProgram
               , lpSolve
               , maximize
               , minimize
               , constrain
               , setFree
               ) where

import           Control.Monad.Writer
import           Data.List            (intercalate)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ()
import           System.Process
import           Text.Printf

data LP = LP { dir          :: Maybe String
             , constrains   :: [String]
             , bounds       :: [String]
             } deriving (Eq, Show)

instance Monoid LP where
    mempty = LP Nothing [] []

    (LP Nothing b c) `mappend` (LP d e f) = LP d (b++e) (c++f)
    (LP a b c) `mappend` (LP Nothing e f) = LP a (b++e) (c++f)
    (LP a b c) `mappend` (LP d e f)
        | a == d = LP a (b++e) (c++f)
        | otherwise = error "Can not combine two directions"

type LinearProgram = Writer LP ()

-- |Set a variable to maximize.
--
maximize :: String -> LinearProgram
maximize v = tell $ LP (Just $ "max: "++v++";") [] []

-- |Set utility function as pairs (coefficient,variable).
--
minimize :: [(Double, String)] -> LinearProgram
minimize vs = tell $ LP (Just $ "min: "++concatMap mult vs ++ ";") [] []

-- |Set variables to be unbounded. By default all variables must be
-- non-negative.
--
setFree :: [String] -> LinearProgram
setFree = tell . LP Nothing []

-- |Set up constrains. First argument is the operator, second argument is left
-- hand side as pairs (coefficient, variable), third argument is the single
-- number on right hand side.
--
constrain :: String -> [[(Double, String)]] -> [Int] -> LinearProgram
constrain op lhs rhs = tell $ (\x -> LP Nothing [x] []) $ unlines $ map go $ zip lhs rhs
  where
    go (l, r) = concatMap mult l ++ op ++ show r ++ ";"

mult :: (Double, String) -> String
mult (n, v)
  | n == 0 = ""
  | n > 0  = '+':p n ++ v
  | n < 0  = p n ++ v
  where
    p = printf "%.5f"
mult (_, _) = error "How is this even possible?"

parse :: String -> (Double, [(String, Double)])
parse s = (opt, vars)
  where
    ls = lines s
    opt = read $ last $ words $ ls !! 1
    vars = map toVar $ drop 4 ls

    toVar l = case words l of
        [var, val] -> (var, read val)
        _ -> error "Unexpected line"

-- |Run given linear program and return value of objective function and values
-- for all variables.
--
lpSolve :: LinearProgram -> IO (Double, [(String, Double)])
lpSolve = liftM parse . readProcess "lp_solve" [] . go . snd . runWriter
  where
    go lp = unlines $ fromMaybe err (dir lp) : constrains lp ++ [bds $ bounds lp]
    err = error "Missing optimization direction"
    bds vs = if null vs then "" else "free " ++ intercalate ", " vs ++ ";"

