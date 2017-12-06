module Lib where
    -- ( someFunc
    -- ) where

import ExprT (ExprT (..))
import Parser
--import StackVM

someFunc :: IO ()
someFunc = do
  putStrLn "test of problem 4\n=================="
  print testInteger
  print testBool
  print testMM
  print testSat
  putStrLn "===================\n"


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
