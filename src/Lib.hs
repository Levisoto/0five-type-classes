module Lib where
    -- ( someFunc
    -- ) where

import ExprT
import Parser
import StackVM

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-------------------------------------------------------------------------
  -- problem 1
-------------------------------------------------------------------------

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

-------------------------------------------------------------------------

-------------------------------------------------------------------------
  -- problem 2
-------------------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr xs 
  | val /= Nothing = fmap eval val
  | otherwise = Nothing
    where
      val = parseExp ExprT.Lit ExprT.Add ExprT.Mul xs

-------------------------------------------------------------------------

-------------------------------------------------------------------------
  -- problem 3
-------------------------------------------------------------------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id
