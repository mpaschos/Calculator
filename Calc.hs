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
      result = op (action) 4 3--(read left :: Integer) (read right :: Integer)


isValidNumber val
  | any (== True) [isValidInt val, isValidDouble val] = True
  | otherwise = False

isValidInt val
  | (all isDigit val) && (not $ hasDot val) = True
  | otherwise = False

isValidDouble val
  | (hasDot val) && (all isDigit $ filter (/='.') val) = True
  | otherwise = False              

hasDot val
  | (length $ filter (=='.') val) == 1 = True
  | otherwise = False
  
trimSpaces :: String -> String
trimSpaces expr = filter (not . isSpace) expr


parseLeftTerm expr
  | null expr = Nothing
  | expr == [] = Nothing
  | otherwise = Just (trimSpaces $ takeWhile (/='+') expr)


parseRightTerm expr
  | null expr = Nothing
  | expr == [] = Nothing
  | otherwise = Just (trimSpaces $ tail $ dropWhile (/='+') expr)

parseOp expr
  | null expr = Nothing
  | expr == [] = Nothing
  | otherwise = Just (head $ trimSpaces $ dropWhile (/='+') expr)


strToInt term 
 | isValidInt term = Just (read term :: Integer)
 | otherwise = Nothing     

strToDouble term 
 | isValidDouble term = Just (read term :: Double)
 | otherwise = Nothing    

main = do
  calculator