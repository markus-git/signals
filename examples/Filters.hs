{-# LANGUAGE TypeOperators #-}

module Examples.Filters where

import Core
import Language.C.Monad
import Language.Embedded.Expr
import Language.Embedded.Backend.C
import Text.PrettyPrint.Mainland

import Frontend.Signal (Sig)
import Frontend.Stream (Stream(..), Str)
import Backend.Compiler.Compiler

import qualified Frontend.Signal as S
import qualified Frontend.Stream as Str
import qualified Text.Printf     as Printf

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type CMD
  =   RefCMD      E
  :+: ControlCMD  E
  :+: FileCMD     E
  :+: ControlCMD  E

type E   = Expr      -- short-hand for expression
type S   = Sig CMD   --      --||--    signals

--------------------------------------------------------------------------------
-- ** FIR Filter

fir :: [E Float] -> S Float -> S Float
fir as = sums . muls as . delays ds
  where ds = replicate (length as) 0

sums :: [S Float] -> S Float
sums = foldr1 (+)

muls :: [E Float] -> [S Float] -> [S Float]
muls as = zipWith (*) (map S.repeat as)

delays :: [E Float] -> S Float -> [S Float]
delays as s = scanl (flip S.delay) s as

--------------------------------------------------------------------------------
-- ** IIR Filter

iir :: [E Float] -> [E Float] -> S Float -> S Float
iir (a:as) bs s = o
  where
    u = fir bs s
    l = fir as $ S.delay 0 o
    o = (1 / S.repeat a) * (u - l)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type Prog = Program CMD 

connect_io :: (S Float -> S Float) -> IO (Prog ())
connect_io s = do
  prog <- compiler s
  return $ do
    inp <- fopen "input"  ReadMode
    out <- fopen "output" WriteMode
    ref <- newRef :: Prog (Ref Float)

    let stream = Str.run $ prog
               $ Stream  $ return
               $ getRef ref

    let cont = fmap Not $ feof inp :: Prog (E Bool)

    while cont $ do
      v <- fget inp
      setRef ref v
      o <- stream
      fput out o

    fclose inp
    fclose out

--------------------------------------------------------------------------------

compFIR :: IO Doc
compFIR = do
  p <- connect_io $ fir [1,2]
  return $ prettyCGen $ wrapMain $ interpret p
 

compIIR :: IO Doc
compIIR = do
  p <- connect_io $ iir [1,2] [3,1]
  return $ prettyCGen $ wrapMain $ interpret p

--------------------------------------------------------------------------------
