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

--Main loop - REPL
calculator = do
  x <- prompt "calc>"
  
  --In case of valid parsing print the result without the 'Just', else print 'Parsing error'
  case (eval x) of
    Just n  -> print $ n
    Nothing -> print $ "Parsing error"
  
  --Recursive call to calculator
  calculator


op :: Fractional a => Char -> a -> a -> a
op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = (/)


nameOfOp :: Char -> String
nameOfOp '+' = "ADD"
nameOfOp '-' = "SUB"
nameOfOp '*' = "MUL"
nameOfOp '/' = "DIV"


isOperator :: Char -> Bool
isOperator op
  | op == '+' = True
  | op == '-' = True
  | op == '*' = True
  | op == '/' = True
  | otherwise = False



eval expr
  | not $ containsLetters expr = result  
  | otherwise = Nothing      
    where
      action = head $ trimSpaces $ dropWhile (not . isOperator) expr
      result = op action <$> parseLeftTerm expr <*> parseRightTerm expr


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
  
containsLetters val
  | filter (isLetter) val /= "" = True
  | otherwise = False


trimSpaces :: String -> String
trimSpaces expr = filter (not . isSpace) expr

strToInt :: String -> Integer
strToInt term = read term :: Integer

strToDouble :: String -> Double
strToDouble term = read term :: Double   

parseLeftTerm expr
  | null expr = Nothing
  | expr == [] = Nothing
  | otherwise = parseNum (trimSpaces $ takeWhile (not . isOperator) expr)


parseRightTerm expr
  | null expr = Nothing
  | expr == [] = Nothing
  | otherwise = parseNum (trimSpaces $ tail $ dropWhile (not . isOperator) expr)


parseOp expr
  | null expr = Nothing
  | expr == [] = Nothing
  | otherwise = Just (head $ trimSpaces $ dropWhile (not . isOperator) expr)


parseNum term
  | isValidNumber term = Just (strToDouble term)
  | otherwise = Nothing











main = do
  calculator