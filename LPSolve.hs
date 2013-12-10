module LPSolve ( LinearProgram
               , lpSolve
               , maximize
               , minimize
               , constrain
               , setFree
               ) where

import           Control.Monad.Reader
import           Data.List            (intercalate)
import           System.Directory     (removeFile)
import           System.IO
import           System.Process
import           Text.Printf

type LinearProgram = ReaderT Handle IO ()

-- |Set a variable to maximize.
--
maximize :: String -> LinearProgram
maximize v = do
    h <- ask
    liftIO $ hPutStr h "max: " >> hPutStr h v >> hPutStrLn h ";"

-- |Set utility function as pairs (coefficient,variable).
--
minimize :: [(Double, String)] -> LinearProgram
minimize vs = do
    h <- ask
    liftIO $ hPutStr h "min: " >> mapM (mult h) vs >> hPutStrLn h ";"

-- |Set variables to be unbounded. By default all variables must be
-- non-negative.
--
setFree :: [String] -> LinearProgram
setFree vs = do
    h <- ask
    liftIO $ hPutStr h $ "free " ++ intercalate ", " vs ++ ";"

-- |Set up constrains. First argument is the operator, second argument is left
-- hand side as pairs (coefficient, variable), third argument is the single
-- number on right hand side.
--
constrain :: String -> [[(Double, String)]] -> [Int] -> LinearProgram
constrain op lhs rhs = do
    h <- ask
    liftIO $ mapM_ (go h) $ zip lhs rhs
  where
    go h (l, r) = mapM_ (mult h) l >> hPutStr h op >> hPrint h r >> hPutStrLn h ";"

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
    (name,h) <- openTempFile "/tmp" "machacek.lp"
    runReaderT prog h
    hClose h
    res <- readProcess "lp_solve" [name] ""
    removeFile name
    return $ parse res
