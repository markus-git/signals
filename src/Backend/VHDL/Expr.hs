{-# LANGUAGE GADTs #-}

module Backend.VHDL.Expr where

import Data.Dynamic (Dynamic, fromDynamic)
import Data.Typeable

import Backend.VHDL.Pretty
import Backend.VHDL.Generate hiding (not)
import Backend.VHDL.Syntax          (Identifier)
import qualified Backend.VHDL.Generate as G
import qualified Backend.VHDL.Syntax   as S

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- ! VHDL types are required, left as 'a' for now
-- ! Expressions poorly translate to a single data type
data Expr a
  where
    Var  :: Identifier -> Expr Bool

    -- logical operators
    Not  :: Expr Bool -> Expr Bool
    And  :: Expr Bool -> Expr Bool -> Expr Bool
    Or   :: Expr Bool -> Expr Bool -> Expr Bool
    Xor  :: Expr Bool -> Expr Bool -> Expr Bool
    Xnor :: Expr Bool -> Expr Bool -> Expr Bool
    Nand :: Expr Bool -> Expr Bool -> Expr Bool
    Nor  :: Expr Bool -> Expr Bool -> Expr Bool

    -- relational operators
    Eq   :: Expr a -> Expr a -> Expr a
    Neq  :: Expr a -> Expr a -> Expr a
    -- ...

---------------------------------------------------------------------------------

evalExpr :: (Identifier -> Dynamic) -> Expr a -> a
evalExpr env exp = case exp of
  Var  v | Just a <- fromDynamic (env v) -> a
  Not  x   -> not $ evalExpr env x
  And  x y -> evalExpr env x && evalExpr env y
  Or   x y -> evalExpr env x || evalExpr env y
  Xor  x y -> evalExpr env x `xor` evalExpr env y
  Xnor x y -> not $ evalExpr env x `xor` evalExpr env y
  Nand x y -> not $ evalExpr env x && evalExpr env y
  Nor  x y -> not $ evalExpr env x || evalExpr env y
  where
    xor a b = (a || b) && not (a && b)

--------------------------------------------------------------------------------

compExpr :: Expr a -> LLVM S.Expression
compExpr exp = case exp of
  Var  v   -> return $ dummy v
  Not  x   -> un  G.not  x
  And  x y -> bin G.and  x y
  Or   x y -> bin G.or   x y
  Xor  x y -> bin G.xor  x y
  Xnor x y -> bin G.xnor x y
  Nand x y -> bin G.nand x y
  Nor  x y -> bin G.nor  x y
  where
    un  f x = do
      a <- compExpr' x
      return $ f a
    bin f x y = do
      a <- compExpr' x
      b <- compExpr' y
      return $ f [a, b]

compExpr' :: Expr a -> LLVM S.Expression
compExpr' x = dummy <$> compExpr x

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
{-
test :: IO ()
test =
  do let (head, body) = runLLVM (behavioural "even") $
           do a <- input "a"
              b <- input "b"
              c <- input "c"
              output "o"
     putStrLn $ show $ pp head
     putStrLn $ show $ pp body

input  :: String -> LLVM (Expr Bool)
input  str = Var <$> signal str S.In  std_logic Nothing

output :: String -> LLVM (Identifier)
output str = signal str S.Out std_logic Nothing
-}
--------------------------------------------------------------------------------
