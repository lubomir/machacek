module LPSolve ( LinearProgram
               , lpSolve
               , maximize
               , constrain
               , setFree
               ) where

import           Control.Monad.Writer
import           Data.List            (intercalate)
import           Data.Monoid          ()
import           System.Process
import           Text.Printf

data LP = LP { dir :: Maybe String
             , bounds :: [String]
             , constrains :: [String]
             } deriving (Eq, Show)

instance Monoid LP where
    mempty = LP { dir = Nothing, bounds = [], constrains = [] }

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
constrain op lhs rhs = mapM_ go $ zip lhs rhs
  where
    go (l, r) = tell $ (\x -> LP Nothing [x] []) $ concatMap f l ++ op ++ show r ++ ";"
    f (n, v)
      | n == 0 = ""
      | n > 0  = '+':p n ++ v
      | n < 0  = p n ++ v
    f (_, _) = error "How is this even possible?"
    p = printf "%.5f"

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
    go (LP Nothing _ _) = error "Missing optimization direction"
    go (LP (Just d) cs vs) =
        unlines $ d : cs ++ ["free " ++ intercalate ", " vs ++ ";"]

