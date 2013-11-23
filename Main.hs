module Main where

import GameTree

import System.Environment

main :: IO ()
main = do
    [arg] <- getArgs
    putStr $ showTree $ mkTree $ read arg
