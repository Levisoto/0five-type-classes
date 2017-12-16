{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Lib where
    -- ( someFunc
    -- ) where

import ExprT (ExprT (..))
import Parser
import StackVM
import qualified Data.Map as M

someFunc :: IO ()
someFunc = do
  putStrLn "test of problem 4\n=================="
  print testInteger
  print testBool
  print testMM
  print testSat
  putStrLn "===================\n"
  putStrLn "test of problem 6\n=================="
  print (add (lit 3) (var "x") :: VarExprT)
  print (withVars [("x", 6)] $ add (lit 3) (var "x"))
  print (withVars [("x", 6)] $ add (lit 3) (var "y"))
  print (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")))


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

-------------------------------------------------------------------------

-------------------------------------------------------------------------
  -- problem 4
-------------------------------------------------------------------------

instance Expr Integer where
  lit x = x
  add x y = (+) x y
  mul x y = (*) x y

instance Expr Bool where
  lit x = (>) x 0
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y)= MinMax $ max x y
  mul (MinMax x) (MinMax y)= MinMax $ min x y

instance Expr Mod7 where
  lit x = Mod7 $ if (x<=0) then 0 else mod x 7
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-------------------------------------------------------------------------

-------------------------------------------------------------------------
  -- problem 5
-------------------------------------------------------------------------

instance Expr StackVM.Program where
  lit i = [StackVM.PushI i]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]


compile :: String -> Maybe StackVM.Program
compile x = parseExp lit add mul x

-------------------------------------------------------------------------

-------------------------------------------------------------------------
  -- problem 6
-------------------------------------------------------------------------

class HasVars a where
  var :: String -> a

data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var String
              deriving (Show,Eq)

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\_ -> Just x)
  add x y = (\vs -> (+) <$> (x vs) <*> (y vs))
  mul x y = (\vs -> (*) <$> (x vs) <*> (y vs))
  -- add a b m = (+) <$> a m <*> b m
  -- mul a b m = (*) <$> a m <*> b m
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
