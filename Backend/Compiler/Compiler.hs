{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}

module Backend.Compiler.Compiler (
    compiler
  , inspect_compiler
  )
where

import           Core (CMD)
import qualified Core as C

import           Frontend.Stream (Stream, Str)
import qualified Frontend.Stream as Str

import           Frontend.Signal (Signal, Sig, Struct(..), TStruct(..), Empty)
import qualified Frontend.Signal as S

import Frontend.SignalObsv (TSignal(..), Node, edges)

import Backend.Ex
import Backend.Compiler.Cycles
import Backend.Compiler.Linker
import Backend.Compiler.Sorter

import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Operational

import Data.Typeable
import Data.Reify (Unique, Graph(..), reifyGraph)
import Data.Maybe (fromJust)
import Data.List  (sortBy, mapAccumR)
import Data.Function (on)

import           Data.Map (Map, (!))
import qualified Data.Map as M

import Prelude hiding (reads)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Shorthand for programs using 'CMD' as their instruction set
type Prog exp = Program (CMD exp)

--------------------------------------------------------------------------------
-- * Channels
--------------------------------------------------------------------------------

-- | Binary trees over references
data RStruct exp a
  where
    RLeaf :: Typeable a => C.Ref (exp a) -> RStruct exp (Empty (exp a))
    RPair :: RStruct exp a -> RStruct exp b -> RStruct exp (a, b)

-- | Untyped binary trees over references
type REx exp = Ex (RStruct exp)

-- | ...
data Channel symbol exp = C {
    _ch_in  :: Map symbol (REx exp)
  , _ch_out :: Map symbol (REx exp)
  }

--------------------------------------------------------------------------------

-- |
initChannels :: Resolution s e -> Prog e (Channel s e)
initChannels res = do
  ins  <- M.traverseWithKey (const makeChannel) $ _input  res
  outs <- M.traverseWithKey (const makeChannel) $ _output res
  return $ C {
    _ch_in  = ins
  , _ch_out = outs
  }

-- |
makeChannel :: TEx e -> Prog e (REx e)
makeChannel (Ex s) = makes s >>= return . Ex
  where
    makes :: TStruct e a -> Prog e (RStruct e a)
    makes (TLeaf _)   = C.initRef >>= return . RLeaf
    makes (TPair r l) = do
      r' <- makes r
      l' <- makes l
      return $ RPair r' l'

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

-- | ...
data Enviroment symbol exp = Env
  { _links    :: Resolution symbol exp
  , _channels :: Channel    symbol exp 
  , _firsts   :: Map symbol (Ex (C.Ref :*: exp)) -- merge with _channels
  , _inputs   :: Ex (Prog exp :*: exp)
--, ...
  }

-- | 
type Type exp = ReaderT (Enviroment Unique exp) (Prog exp)

--------------------------------------------------------------------------------

reads :: RStruct exp a -> Prog exp (Struct exp a)
reads (RLeaf r)   = C.unsafeGetRef r >>= return . Leaf
reads (RPair l r) = do
  l' <- reads l
  r' <- reads r
  return $ Pair l' r'

writes :: Struct exp a -> RStruct exp a -> Prog exp ()
writes (Leaf s)   (RLeaf r)   = C.setRef r s
writes (Pair l r) (RPair u v) = writes l u >> writes r v

-- | Read
read_in :: Typeable a => Unique -> TStruct exp a -> Type exp (Struct exp a)
read_in u _ =
  do (Ex ch) <- asks ((! u) . _ch_in . _channels)
     case gcast ch of
       Just s  -> lift $ reads s
       Nothing -> error "hepa: type error"

-- | Read 
read_out :: Typeable a => Unique -> TStruct exp a -> Type exp (Struct exp a)
read_out u _ =
  do (Ex ch) <- asks ((! u) . _ch_out . _channels)
     case gcast ch of
       Just s  -> lift $ reads s
       Nothing -> error "!"

-- | Write
write_out :: Typeable a => Unique -> Struct exp a -> Type exp ()
write_out u s =
  do (Ex ch) <- asks ((! u) . _ch_out . _channels)
     case gcast ch of
       Just r  -> lift $ writes s r
       Nothing -> error "depa: type error"

--------------------------------------------------------------------------------

-- | ...
compile :: (Unique, Node exp) -> Type exp ()
compile (i, TVar t@(TLeaf _)) =
  do input <- asks (apa t . _inputs)
     value <- lift $ liftProgram input
     write_out i (Leaf value)
  where
    apa :: Typeable e => TStruct exp (Empty (exp e)) -> Ex (f :*: g) -> f (g e)
    apa _ = unwrap

compile (i, TConst c) =
  do value <- lift $ liftProgram $ Str.run c
     write_out i (Leaf value)

compile (i, TLift (f :: Stream exp (exp a) -> Stream exp (exp b)) _) =
  do let t = undefined :: TStruct exp (Empty (exp a))
     (Leaf input) <- read_in i t
     value <- lift $ liftProgram $ Str.run $ f $ Str.repeat input
     write_out i (Leaf value)
     
compile (i, TDelay (e :: exp a) _) =
  do let t = undefined :: TStruct exp (Empty (exp a))
     (Leaf input) <- read_in i t
     first <- asks (unwrap . (! i) . _firsts) :: Type exp (C.Ref (exp a))
     value <- lift $ liftProgram $
                do output <- C.unsafeGetRef first
                   C.setRef first input
                   return output
     write_out i (Leaf value)

compile (i, TMap ti to f _) =
  do input <- read_in i ti
     value <- return $ f input
     write_out i value
     
compile _ = return ()

--------------------------------------------------------------------------------

-- | ...
compiler' :: forall exp a b. (Typeable exp, Typeable a, Typeable b)
          => [(Unique, Node exp)]
          -> Resolution Unique exp
          -> Map Unique Order
          -> (Stream exp (exp a) -> Stream exp (exp b))
compiler' nodes links order input = Str.stream $
  do let sorted = sort  nodes
     let last   = final sorted
     env <- init (Str.run input)
     return $
       do let t = undefined :: TStruct exp (Empty (exp b))
          (Leaf value) <- run env (mapM_ compile sorted >> read_out last t)
          return value

  where
    run :: Enviroment Unique exp -> Type exp x -> Prog exp x
    run = flip runReaderT

    -- Create initial eviroment
    init :: Prog exp (exp a) -> Prog exp (Enviroment Unique exp)
    init i =
      do let m = M.fromList [ x | x@(_, TDelay {}) <- nodes]
         firsts   <- M.traverseWithKey (const $ init_delay) m
         channels <- initChannels links
         return $ Env {
             _links    = links
           , _channels = channels
           , _firsts   = firsts
           , _inputs   = wrap i
           }
      where
        init_delay (TDelay d _) = C.newRef d >>= return . wrap
        
    -- Sort graph nodes by the given ordering
    sort :: [(Unique, Node exp)] -> [(Unique, Node exp)]
    sort = fmap (fmap snd) . sortBy (compare `on` (fst . snd))
         . M.toList . M.intersectionWith (,) order
         . M.fromList

    -- Find final reference to read output from
    final :: [(Unique, Node exp)] -> Unique
    final = fst . last . filter (not . nop . snd)
      where nop (TLambda {}) = True
            nop (TZip {})    = True
            nop (TFst {})    = True
            nop (TSnd {})    = True
            nop _            = False

--------------------------------------------------------------------------------

compiler :: (Typeable exp, Typeable a, Typeable b)
         =>    (Sig exp a -> Sig exp b)
         -> IO (Str exp a -> Str exp b)
compiler f =
  do (Graph nodes root) <- reifyGraph f

     let links = linker nodes
         order = sorter root nodes
         cycle = cycles root nodes

     return $ case cycle of
       True  -> error "found cycle in graph"
       False -> compiler' nodes links order

--------------------------------------------------------------------------------
-- * Buffers
--------------------------------------------------------------------------------

data Buffer exp a = Buffer
  { getBuff :: exp Int -> Program (CMD exp) (exp a)
  , putBuff :: exp a   -> Program (CMD exp) ()
  }

newBuff
  :: forall exp a. (Num (exp Int), Integral (exp Int))
  => exp Int -> exp a -> Program (CMD exp) (Buffer exp a)
newBuff size init =
  do arr <- C.newArr size init
     ir  <- C.newRef (0 :: exp Int)
     let get j = do i <- C.unsafeGetRef ir
                    C.getArr ((i + (size - j - 1)) `mod` size) arr
     let put a = do i <- C.unsafeGetRef ir
                    C.setArr i a arr
                    -- missing iff
     return $ Buffer get put

--------------------------------------------------------------------------------


opt_delay_chains :: (Typeable e, Typeable a, Num (e Int))
  => Map Unique (Node e)
  -> ( Map Unique (Node e)
     , Map Unique [e a])
opt_delay_chains nodes =
  let x = find_chains nodes
      (y, buffs) = buffer_chains x
      z = merge_chains nodes y
  in  (z, buffs)

find_chains :: Map Unique (Node e) -> Map Unique [(Unique, Node e)]
find_chains nodes =
  let delays = M.foldrWithKey (\k n -> M.insert (edge n) (k, n)) M.empty
             $ M.filter isDelay nodes
      heads  = M.foldr (M.delete . fst) delays delays
  in  M.map (flip chain delays) heads
  where
    chain :: (Unique, Node e) -> Map Unique (Unique, Node e) -> [(Unique, Node e)] 
    chain v@(i, _) m = v : maybe [] (flip chain m) (M.lookup i m)

    edge :: Node e -> Unique
    edge = head . edges

    isDelay :: Node e -> Bool
    isDelay (TDelay {}) = True
    isDelay _           = False
  
buffer_chains
  :: forall e a. (Typeable e, Typeable a, Num (e Int))
  => Map Unique [(Unique, Node e)]
  -> ( Map Unique [(Unique, Node e)]
     , Map Unique [e a])
buffer_chains chains =
  let vals  = M.map (map val) chains
      buffs = M.mapWithKey (\k -> snd . mapAccumR (acc k) 1) chains
  in (buffs, vals)
  where
    val     (i, TDelay v _) = fromJust $ cast v
    acc k n (i, TDelay v _) = (n+1, (i, TDBuff k $ fromIntegral n))

merge_chains
  :: Map Unique (Node e)
  -> Map Unique [(Unique, Node e)]
  -> Map Unique (Node e)
merge_chains nodes chains =
  let nodes' = M.foldr (flip $ foldr (\(u,n) -> M.adjust (const n) u)) nodes chains
      heads' = M.foldrWithKey (\k _ -> M.insert (-k) (TVBuff k)) M.empty chains
  in  M.union nodes' heads'

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

inspect_compiler :: (Typeable exp, Typeable a, Typeable b, Num (exp Int))
                 =>    (Sig exp a -> Sig exp b)
                 -> IO (Str exp a -> Str exp b)
inspect_compiler f =
  do (Graph nodes root) <- reifyGraph f

     let links = linker nodes
         order = sorter root nodes
         cycle = cycles root nodes

   --let chains = M.map (const "Buff.") $ snd $ opt_delay_chains $ M.fromList nodes
     
     putStrLn "=================================================="
     putStrLn "= Inspecting Compiler"
     putStrLn "=================================================="
     putStrLn "- Nodes"
     putStrLn "--------------------------------------------------"
     putStrLn $ show nodes
     putStrLn "--------------------------------------------------"
     putStrLn "- Order"
     putStrLn "--------------------------------------------------"
     putStrLn $ show order
     putStrLn "--------------------------------------------------"
     putStrLn "- Input Links"
     putStrLn "--------------------------------------------------"
     putStrLn $ show $ _input links
     putStrLn "--------------------------------------------------"
     putStrLn "- Output Links"
     putStrLn "--------------------------------------------------"
     putStrLn $ show $ _output links
     putStrLn "--------------------------------------------------"

{-
     putStrLn "- Buffers"
     putStrLn "--------------------------------------------------"
     putStrLn $ show $ chains
     putStrLn "--------------------------------------------------"
-}
     
     return $ case cycle of
       True  -> error "found cycle in graph"
       False -> compiler' nodes links order

--------------------------------------------------------------------------------
                                 
m !? i = case M.lookup i m of
           Just x  -> x
           Nothing -> error $ "Can't find key " ++ show i ++
                              " in map: \n"     ++ show m
