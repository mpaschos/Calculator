module Main where

import Control.Monad
import Data.Char
import System.IO
import Data.Typeable


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


op :: Fractional a => Char -> a -> a -> a
op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = (/)


calculator = do
  x <- prompt "calc>"
  putStrLn x
  print $ tokenize x
  calculator



tokenize expr
  | isValidNumber (show result) == True = Just result  
  | otherwise = Nothing      
    where
      left = trimSpaces $ takeWhile (/='+') expr
      right = trimSpaces $ tail $ dropWhile (/='+') expr
      action = head $ trimSpaces $ dropWhile (/='+') expr
      result = (read left :: Integer) + (read right :: Integer)


isValidNumber val
  | all isNumber val == True = True
  | otherwise = False


trimSpaces :: String -> String
trimSpaces expr = filter (not . isSpace) expr






main = do
  calculator