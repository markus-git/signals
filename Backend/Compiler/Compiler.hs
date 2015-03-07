{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleContexts    #-}

module Backend.Compiler.Compiler (
    compiler
  , inspect_compiler
  )
where

import           Core (CMD)
import qualified Core as C

import           Frontend.Stream (Stream, Str)
import qualified Frontend.Stream as Str

import           Frontend.Signal (Sig, Struct(..), TStruct(..), Empty)
import qualified Frontend.Signal as S

import Frontend.SignalObsv (TSignal(..), Node)

import Backend.Compiler.Cycles
import Backend.Compiler.Linker
import Backend.Compiler.Sorter

import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Operational

import Data.Typeable
import Data.Reify (Unique, Graph(..), reifyGraph)
import Data.Maybe (fromJust)
import Data.List  (sortBy)
import Data.Function (on)

import           Data.Map (Map, (!))
import qualified Data.Map as M

import Prelude hiding (reads)

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

-- | Shorthand for programs using 'CMD' as their instruction set
type Prog exp = Program (CMD exp)

-- | Untyped binary trees over references
type REx exp = Ex (RStruct exp)

-- | Binary trees over references
data RStruct exp a
  where
    RLeaf :: Typeable a => C.Ref (exp a) -> RStruct exp (Empty (exp a))
    RPair :: RStruct exp a -> RStruct exp b -> RStruct exp (a, b)

--------------------------------------------------------------------------------

-- | ...
data Enviroment symbol exp = Env
  { _links   :: Resolution symbol exp
  , _buffers :: forall a. Map symbol (Buffer exp a) -- todo
  , _firsts  :: Map symbol (Bepa exp)
  , _inputs  :: Cepa exp 
--, ...
  }

-- | Mapping from port id to its associated reference
type State symbol exp = Map symbol (REx exp)

-- | 
type Type exp = ReaderT (Enviroment Unique exp)
                  (StateT (State String exp)
                     (Prog exp))

--------------------------------------------------------------------------------

-- | ...
apa :: (Typeable exp) => TStruct exp a -> Type exp (RStruct exp a)
apa (TLeaf i) =
  do ex <- gets (!? i)
     return $ case ex of
       Ex ref -> case gcast ref of
         Nothing -> error "apa: type error"
         Just r  -> r
apa (TPair l r) =
  do l' <- apa l
     r' <- apa r
     return $ RPair l' r'

-- | ...
bepa :: RStruct exp a -> Type exp (Struct exp a)
bepa (RLeaf r)   = lift (lift (C.unsafeGetRef r)) >>= return . Leaf
bepa (RPair l r) =
  do l' <- bepa l
     r' <- bepa r
     return $ Pair l' r'

-- | Read
cepa :: (Typeable exp, Typeable a) => Unique -> Struct exp a -> Type exp (Struct exp a)
cepa i t =
  do (Ex ts) <- asks ((! i) . _input . _links)
     rs      <- apa ts
     ss      <- bepa rs
     return $ case gcast ss of
       Nothing -> error "cepa: type error"
       Just o  -> o `asTypeOf` t

-- | ...
depa :: Struct exp a -> Type exp (RStruct exp a)
depa (Leaf a)   = lift (lift (C.newRef a)) >>= return . RLeaf
depa (Pair l r) =
  do l' <- depa l
     r' <- depa r
     return $ RPair l' r'

-- | Write
hepa :: (Typeable a) => String -> Struct exp a -> Type exp ()
hepa u s = depa s >>= \n -> modify (M.insert u $ Ex n)

--------------------------------------------------------------------------------

-- | ...
compile :: forall exp. (Unique, Node exp) -> Type exp ()
compile (i, TVar (_ :: TStruct exp a)) =
  do let t = undefined :: Struct exp a
     (Cepa x) <- asks (_inputs)
     v <- lift $ lift $ liftProgram x
     hepa (show i) ((fromJust $ gcast $ Leaf v) `asTypeOf` t)
     
compile (i, TConst (c :: Stream exp (exp a))) =
  do v <- lift $ lift $ liftProgram $ Str.run c
     hepa (show i) (Leaf v)

compile (i, TLift (f :: Stream exp (exp a) -> Stream exp (exp b)) _) =
  do let t = undefined :: Struct exp (exp a)
     (Leaf x) <- cepa i t
     v <- lift $ lift $ liftProgram $ Str.run $ Str.repeat x
     hepa (show i) (Leaf v)
     
-- hackity hack
compile (i, TDelay (e :: exp a) _) =
  do let t = undefined :: Struct exp (Empty (exp a))
         u = undefined :: C.Ref (exp a)
     (Bepa r) <- asks ((! i) . _firsts)
     (Leaf x) <- cepa i t
     v <- lift $ lift $
            do o <- C.unsafeGetRef r
               C.setRef r $ fromJust $ cast x
               return o
     hepa (show i) (Leaf v)
     
compile (i, TMap _ _ (f :: Struct exp a -> Struct exp b) _) =
  do let t = undefined :: Struct exp a
     t' <- cepa i t
     hepa (show i) (f t')
     
compile _ = return ()

--------------------------------------------------------------------------------

-- just work already...
data Apa e where
  Apa :: Typeable a => e a -> Apa e

data Bepa e where
  Bepa :: Typeable a => C.Ref (e a) -> Bepa e

data Cepa e where
  Cepa :: Typeable a => Prog e (e a) -> Cepa e

-- | ...
compiler' :: forall e a b. (Typeable e, Typeable a, Typeable b)
          => [(Unique, Node e)]  -- nodes
          -> Resolution Unique e -- links
          -> Map Unique Order    -- order
          -> (Str e a -> Str e b)
compiler' nodes links order input = Str.stream $
  do let ns = sort nodes
     let l  = final ns
     i <- init $ Str.run input
     return $
       do m <- run i $ mapM_ compile ns
          case m ! show l of
            Ex (RLeaf a) -> fromJust $ gcast $ C.unsafeGetRef a
  where
    run :: Enviroment Unique e -> Type e () -> Prog e (State String e)
    run e = flip execStateT M.empty . flip runReaderT e

    init :: Prog e (e a) -> Prog e (Enviroment Unique e)
    init n =
      do -- Create initial references for delay nodes
         let (is, ds) = unzip [(i, Apa d) | (i, TDelay d _) <- nodes]
         drs <- sequence $ map (\(Apa d) -> C.newRef d >>= return . Bepa) ds

         -- Create enviroment
         return $ Env
           { _links   = links
           , _buffers = M.empty
           , _firsts  = M.fromList $ zip is drs
           , _inputs  = Cepa n
           }

    -- Sort graph nodes by the given ordering
    sort :: [(Unique, Node e)] -> [(Unique, Node e)]
    sort = fmap (fmap snd) . sortBy (compare `on` (fst . snd))
         . M.toList . M.intersectionWith (,) order
         . M.fromList

    final :: [(Unique, Node e)] -> Unique
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
  { getBuff :: exp Int -> Prog exp (exp a)
  , putBuff :: exp a   -> Prog exp ()
  }

instance Show (Buffer e a) where show _ = "buffer. "

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

inspect_compiler :: (Typeable exp, Typeable a, Typeable b)
                 =>    (Sig exp a -> Sig exp b)
                 -> IO (Str exp a -> Str exp b)
inspect_compiler f =
  do (Graph nodes root) <- reifyGraph f

     let links = linker nodes
         order = sorter root nodes
         cycle = cycles root nodes

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
     
     return $ case cycle of
       True  -> error "found cycle in graph"
       False -> compiler' nodes links order

--------------------------------------------------------------------------------
                                 
m !? i = case M.lookup i m of
           Just x  -> x
           Nothing -> error $ "Can't find key " ++ show i ++
                              " in map: \n"     ++ show m
