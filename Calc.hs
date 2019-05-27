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
  -- putStrLn "Expression tokenized: " ++ (show tokenize x)
  print $ tokenize x
  calculator



tokenize expr
  | length expr < 3 = Left ("Ill-formatted expression: " ++ expr)
  | otherwise = Right ("Eval: " ++ show result)
      where
        lexemes = words expr
        left = read (lexemes !! 0) :: Int
        right = read (lexemes !! 2) :: Int
        result = left + right






-- takeInput :: IO [Char] -> IO Maybe Int
takeInput = do
  x <- getChar
  if isNumber x
    then 
      -- num = read $ x :: Int
      return (Just x)
  else
    return Nothing  


main = do
  calculator