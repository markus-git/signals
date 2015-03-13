{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Examples.Filters where

import Core
import Language.C.Monad
import Language.Embedded.Expr
import Language.Embedded.Backend.C
import Text.PrettyPrint.Mainland

import Frontend.Signal (Sig)
import Frontend.Stream (Str)
import Backend.Compiler.Compiler

import qualified Frontend.Signal as S
import qualified Frontend.Stream as Str
import qualified Text.Printf     as Printf

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type E   = Expr    -- short-hand for expression
type S   = Sig E   --      --||--    signals
type Prg = Prog E  --      --||--    programs

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

prl :: (f p e :<: g) => Tag p e g prog a -> Maybe (f p e prog a)
prl = prj . unTag

inl :: (f p e :<: g) => f p e prog a -> Tag p e g prog a
inl = Tag . inj

apa :: ( RefCMD (P exp) exp :<: cmd
       , ControlCMD     exp :<: cmd
       )
    => (Tag (P exp) exp (CMD exp) prog a)
    -> (Tag (P exp) exp cmd prog a)
apa c | Just (NewRef) <- prl c = inl $ NewRef
  --  | Just (If a b c) <- prl c = inl $ If a b c

bepa :: ( RefCMD (P exp) exp :<: cmd
        , ControlCMD     exp :<: cmd
        )
     => Prog exp a -> Program (Tag (P exp) exp cmd) a
bepa = interpretWithMonad (singleton . apa)

--------------------------------------------------------------------------------

type CMD2 exp
  =   RefCMD (P exp) exp
  :+: ControlCMD  exp
  :+: FileCMD     exp
  :+: ConsoleCMD  exp

type Prg2 = Program (Tag (P E) E (CMD2 E)) 

connect_io :: P E Float => (S Float -> S Float) -> IO (Prg2 ())
connect_io s = do
  prog <- compiler s
  return $ do
    inp <- open "input"
    out <- open "output"
    ref <- newRef :: Prg2 (Ref Float)
    let str  = Str.run $ prog $ Str.stream $ return $ getRef ref :: Prg (E Float)
       
        p    = bepa str :: Program (Tag (P E) E (CMD2 E)) (E Float)
    let cont = fmap Not $ feof inp
    while cont $ do
      v <- fget inp
      setRef ref v
      o <- p
      fput out o
    close inp
    close out
     
