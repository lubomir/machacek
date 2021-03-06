module LPSolve ( LinearProgram
               , lpSolve
               , OptimizationDir(..)
               , optimize
               , constrain
               , setFree
               ) where

import           Control.Monad.Reader
import           Data.List            (intercalate)
import           System.Directory     (getTemporaryDirectory)
import           System.IO
import           System.Process
import           Text.Printf

type LinearProgram = ReaderT Handle IO ()

data OptimizationDir = Min | Max deriving (Eq, Show)

-- |Set up utility function.
--
optimize :: OptimizationDir -> [(Double, String)] -> LinearProgram
optimize dir vs = ask >>= \h ->
    liftIO $ hPutStr h d >> mapM (mult h) vs >> hPutStrLn h ";"
  where d = case dir of
                Min -> "min: "
                Max -> "max: "

-- |Set variables to be unbounded. By default all variables must be
-- non-negative.
--
setFree :: [String] -> LinearProgram
setFree vs = ask >>= \h ->
    liftIO $ hPutStr h $ "free " ++ intercalate ", " vs ++ ";"

-- |Set up constrains. First argument is the operator, second argument is left
-- hand side as pairs (coefficient, variable), third argument is the single
-- number on right hand side.
--
constrain :: String -> [[(Double, String)]] -> [Int] -> LinearProgram
constrain op lhs rhs = ask >>= \h ->
    liftIO $ mapM_ (go h) $ zip lhs rhs
  where
    go h (l, r) = mapM_ (mult h) l >> hPutStr h op >> hPutStr h (show r) >> hPutStrLn h ";"

{-# INLINE mult #-}
mult :: Handle -> (Double, String) -> IO ()
mult h = uncurry (hPrintf h "%+.5f%s")

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
lpSolve prog = do
    dir <- getTemporaryDirectory
    (name,h) <- openTempFile dir "machacek.lp"
    hPutStrLn stderr $ "Writing linear program to "++name
    runReaderT prog h
    hClose h
    res <- readProcess "lp_solve" [name] ""
    return $ parse res
