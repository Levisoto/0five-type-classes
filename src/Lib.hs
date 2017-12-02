module Lib where
    -- ( someFunc
    -- ) where

import ExprT
import Parser
-- import StackVM

someFunc :: IO ()
someFunc = putStrLn "someFunc"

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr xs 
  | val /= Nothing = fmap eval val
  | otherwise = Nothing
    where
      val = parseExp Lit Add Mul xs
