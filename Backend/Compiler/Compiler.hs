{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}

module Backend.Compiler.Compiler (
    compiler
  )
where

import Core
import Frontend.Stream (Stream, Str)
import Frontend.Signal (Signal, Sig, Struct(..), TStruct(..), Empty)
import Frontend.TSignal 
import Backend.Ex
import Backend.Compiler.Cycles
import Backend.Compiler.Linker
import Backend.Compiler.Sorter
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Data.Reify
import Data.Maybe       (fromJust)
import Data.List        (sortBy, mapAccumR)
import Data.Traversable (traverse)
import Data.Function    (on)
import Data.Map         (Map, (!))
import Data.Constraint
import qualified Core            as C
import qualified Frontend.Stream as Str
import qualified Frontend.Signal as S
import qualified Data.Map        as M

import Prelude hiding (reads)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

compiler :: (Typeable exp, P exp a, P exp b)
         =>    (Sig exp a -> Sig exp b)
         -> IO (Str exp a -> Str exp b)
compiler f =
  do (Graph nodes root) <- reifyGraph f

     let links = linker nodes
         order = sorter root nodes
         cycle = cycles root nodes

     return $ case cycle of
       True  -> error "found cycle in graph"
       False -> undefined -- compiler' nodes links order

--------------------------------------------------------------------------------
-- * Channels
--------------------------------------------------------------------------------

-- | Binary trees over references
data RStruct exp a
  where
    RLeaf :: P exp a => C.Ref a -> RStruct exp (Empty (exp a))
    RPair :: RStruct exp a -> RStruct exp b -> RStruct exp (a, b)

-- | Untyped binary trees over references
type REx exp = Ex (RStruct exp)

-- | ...
data Channel symbol exp = C {
    _ch_in  :: Map symbol (REx exp)
  , _ch_out :: Map symbol (REx exp)
  }

--------------------------------------------------------------------------------
-- hacky solution for now

-- |
initChannels :: (Ord s, Read s, Typeable e) => Resolution s e -> Prog e (Channel s e)
initChannels res = do
  outs <- M.traverseWithKey (const makeChannel) $ _output res
  return $ C {
    _ch_in  = M.map (copyChannel outs) $ _input res
  , _ch_out = outs
  }

-- |
makeChannel :: forall e. TEx e -> Prog e (REx e)
makeChannel (Ex s) = makes s >>= return . Ex
  where
    makes :: TStruct e a -> Prog e (RStruct e a)
    makes x@(TLeaf _) = C.newRef >>= return . RLeaf
    makes (TPair r l) = do
      r' <- makes r
      l' <- makes l
      return $ RPair r' l'

-- |
copyChannel :: forall e s. (Ord s, Read s, Typeable e) => Map s (REx e) -> TEx e -> REx e
copyChannel m (Ex s) = Ex $ copys s
  where
    copys :: TStruct e a -> RStruct e a
    copys (TLeaf i)   = case m ! read i of (Ex (RLeaf r)) -> case gcast r of Just x -> RLeaf x
    copys (TPair l r) = RPair (copys l) (copys r)

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

-- | ...
data Enviroment symbol exp = Env
  { _links    :: Resolution symbol exp
  , _channels :: Channel    symbol exp 
  , _firsts   :: Map symbol (Ex C.Ref) -- merge with _channels
  , _inputs   :: Ex (Prog exp :*: exp)
--, ...
  }

-- | 
type Type exp = ReaderT (Enviroment Unique exp) (Prog exp)

--------------------------------------------------------------------------------

reads :: RStruct exp a -> Prog exp (Struct exp a)
reads (RLeaf r)   = C.unsafeFreezeRef r >>= return . Leaf
reads (RPair l r) = do
  l' <- reads l
  r' <- reads r
  return $ Pair l' r'

writes :: Struct exp a -> RStruct exp a -> Prog exp ()
writes (Leaf s)   (RLeaf r)   = C.setRef r s
writes (Pair l r) (RPair u v) = writes l u >> writes r v

--------------------------------------------------------------------------------

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
       Nothing -> error "bepa: type error"

-- | Write
write_out :: Typeable a => Unique -> Struct exp a -> Type exp ()
write_out u s =
  do (Ex ch) <- asks ((! u) . _ch_out . _channels)
     case gcast ch of
       Just r  -> lift $ writes s r
       Nothing -> error "depa: type error"

--------------------------------------------------------------------------------

-- | ...
compile :: (Typeable exp) => (Unique, Node exp) -> Type exp ()
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

-- I could remove the extra variable (value) if updating all delay
-- values was the last thing I did in the compiler, like for buffers.
compile (i, TDelay (e :: exp a) _) =
  do let t = undefined :: TStruct exp (Empty (exp a))
     (Leaf input) <- read_in i t
     (Ex   first) <- asks ((! i) . _firsts)
     let f = case gcast first of
               Just x  -> x
               Nothing -> error "!"
     value <- lift $ liftProgram $
                do output <- C.unsafeFreezeRef f
                   C.setRef f input
                   return output
     write_out i (Leaf value)

compile (i, TMap ti to f _) =
  do input <- read_in i ti
     value <- return $ f input
     write_out i value
     
compile _ = return ()

--------------------------------------------------------------------------------

-- | ...
compiler' :: forall exp a b. (Typeable exp, P exp a, P exp b)
          => [(Unique, Node exp)]
          -> Resolution Unique exp
          -> Map Unique Order
          -> (Stream exp (exp a) -> Stream exp (exp b))
compiler' nodes links order input = Str.stream $
  do env <- init (Str.run input)
     return $
       do let sorted = sort   nodes
          let last   = final  sorted
          (Leaf value) <- flip runReaderT env
                        $ do let t = undefined :: TStruct exp (Empty (exp b))
                             mapM_ compile sorted
                             read_out last t
          return value

  where
    -- Create initial eviroment
    init :: Prog exp (exp a) -> Prog exp (Enviroment Unique exp)
    init i =
      do let delays = M.fromList [ x | x@(_, TDelay {}) <- nodes] :: Map Unique (Node exp)
             fnodes = map fst $ filterNOP nodes
             flinks = Resolution {
                 _output = M.filterWithKey (\k _ -> k `elem` fnodes) $ _output links
               , _input  = M.filterWithKey (\k _ -> k `elem` fnodes) $ _input  links
               }
         firsts   <- M.traverseWithKey (const $ init_delay) delays
         channels <- initChannels flinks
         return $ Env {
             _links    = links
           , _channels = channels
           , _firsts   = firsts
           , _inputs   = wrap i
           }
      where
        init_delay :: Node exp -> Prog exp (Ex C.Ref)
        init_delay (TDelay d _) = C.initRef d >>= return . Ex
        
    -- Sort graph nodes by the given ordering
    sort :: [(Unique, Node exp)] -> [(Unique, Node exp)]
    sort = fmap (fmap snd) . sortBy (compare `on` (fst . snd))
         . M.toList . M.intersectionWith (,) order
         . M.fromList

    -- Find final reference to read output from
    final :: [(Unique, Node exp)] -> Unique
    final = fst . last . filterNOP

    -- Filter unused nodes
    filterNOP :: [(Unique, Node exp)] -> [(Unique, Node exp)]
    filterNOP = filter (not . nop . snd)
      where nop (TLambda {}) = True
            nop (TZip {})    = True
            nop (TFst {})    = True
            nop (TSnd {})    = True
            nop _            = False



















--------------------------------------------------------------------------------
-- * Buffers
--------------------------------------------------------------------------------
{-
data Buffer exp a = Buffer
  { getBuff :: Program (CMD exp) (exp a)
  , putBuff :: exp a -> Program (CMD exp) ()
  }

newBuff :: forall exp a. (EEq exp Int, Num (exp Int), Integral (exp Int))
        => exp Int -> exp a -> Prog exp (Buffer exp a)
newBuff size init =
  do arr <- C.newArr size init
     ir  <- C.newRef (0 :: exp Int)

     let get = do i <- C.unsafeGetRef ir
                  C.iff (i ==: 0)
                        (C.setRef ir size)
                        (C.setRef ir (i - 1))
                  C.getArr i arr
     
     let put a = do i <- C.unsafeGetRef ir
                    C.setArr i a arr
                    C.iff (i ==: size)
                          (C.setRef ir 0)
                          (C.setRef ir (i + 1))

     return $ Buffer get put

--------------------------------------------------------------------------------

-- | For each node in the given list, it finds any chain of delays associated
--   with the node and returns a mapping over each chained node and its chain
find_chains :: [(Unique, Node e)] -> Map Unique [(Unique, Node e)]
find_chains nodes = 
  let delays = M.foldrWithKey (\k n -> M.insert (head $ edges n) (k, n)) M.empty
             $ M.fromList [ d | d@(i, TDelay {}) <- nodes ]
      heads  = M.foldr (M.delete . fst) delays delays
  in  M.filter ((>1) . length) $ M.map (flip chain delays) heads
  where
    chain v@(i, _) m = v : maybe [] (flip chain m) (M.lookup i m)

-- | 
buffer_chains :: forall e. (EEq e Int, Integral (e Int), Num (e Int))
              => Map Unique [(Unique, Node e)]          -- original chains
              -> Prog e ( Map Unique [(Unique, Node e)] -- updated chains
                        , Map Unique (Ex (Buffer e))    -- buffers
                        )
buffer_chains chains = 
  do let values  = M.map        (map val)   chains
         chains' = M.mapWithKey (map . acc) chains

     -- Since newBuff fills the entire array with the same value,
     -- we only use the first value of the delay chains.
     -- This should be fixed!
     buffers <- traverse (\x@((Ex v):_) ->
                      do buff <- newBuff (fromIntegral $ length x) v
                         return (Ex buff)
                    )
                  values
     
     return (chains', buffers)
  where
    val   (i, TDelay v _) = Ex v
    acc k (i, TDelay v _) = (i, TBuff (apa v) k)
      where apa :: exp a -> Proxy (exp a)
            apa _ = Proxy::Proxy (exp a)

-- | Replaces all original nodes with the updated chain versions
replace_chains :: [(Unique, Node e)] -> Map Unique [(Unique, Node e)] -> [(Unique, Node e)]
replace_chains nodes = M.toList . M.foldr (flip $ foldr $ uncurry M.insert) (M.fromList nodes)

--------------------------------------------------------------------------------

-- | ...
--
-- We assume that:
--   * Each delay chains values are of the same type
--   * ...
opt_delay_chains :: (EEq e Int, Num (e Int), Integral (e Int))
                 => [(Unique, Node e)]
                 -> Prog e ( [(Unique, Node e)]
                           , Map Unique (Ex (Buffer e))
                           )
opt_delay_chains nodes =
  do (chains, buffers) <- buffer_chains $ find_chains nodes
     return (replace_chains nodes chains, buffers)
-}
--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------
{-
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

     return $ \input -> case cycle of
       True  -> error "found cycle in graph"
       False -> compiler' nodes links order input
-}
--------------------------------------------------------------------------------
-- ** Testing Ex
{-
instance Show (Ex c) where show _ = "Ex"

instance Show (Ex (TStruct e))
  where
    show (Ex s) = showTS s

showTS :: TStruct e a -> String
showTS (TLeaf c) = show c
showTS (TPair l r) = "(" ++ showTS l ++ "," ++ showTS r ++ ")"
-}
