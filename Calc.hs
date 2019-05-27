module Calc where

import Control.Monad
import Data.Char
import System.IO


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine



calculator = do
  x <- prompt "calc>"
  putStrLn x
  print $ tokenize x
  calculator


evalOutput result = result


tokenize str
  | length expr < 3 = Left ("Ill-formatted expression: " ++ expr)
  | otherwise = Right ("Eval: " ++ show result)
      where
        expr = trimWhiteSpace str
        lexemes = words expr
        left = read (lexemes !! 0) :: Int
        right = read (lexemes !! 2) :: Int
        result = left + right





trimWhiteSpace :: String -> String
trimWhiteSpace expr
  | null expr = []
  | otherwise = [c| c <- expr, not $ isSpace c]








main = do
  calculator