{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Backend.VHDL.Generate where

import Backend.VHDL.Syntax
import Backend.VHDL.Pretty

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.List (find)

import Prelude hiding (not)

--------------------------------------------------------------------------------
-- * LLVM Generation Monad
--------------------------------------------------------------------------------
-- ! Restricted to a single entity/architecture
-- ! Missing errors

data EntityState = Entity {
    entity_ident             :: String
  , entity_generics          :: Maybe GenericClause
  , entity_ports             :: Maybe PortClause
  , entity_declarative       :: EntityDeclarativePart
  , entity_statements        :: Maybe EntityStatementPart
  }

data ArchitectureState = Architecture {
    architecture_ident       :: String
  , architecture_header      :: EntityState
  , architecture_declarative :: ArchitectureDeclarativePart
  , architecture_statements  :: ArchitectureStatementPart
  }

newtype LLVM a = LLVM { unLLVM :: State ArchitectureState a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState ArchitectureState
           )

--------------------------------------------------------------------------------
-- ** Generating LLVM modules

runLLVM :: ArchitectureState -> LLVM a -> (EntityDeclaration, ArchitectureBody)
runLLVM state m = 
  let (Architecture ident header adecl astmt) = execState (unLLVM m) state
      (Entity    name gens ports edecl estmt) = header
   in ( EntityDeclaration (Ident name) (EntityHeader gens ports) edecl estmt
      , ArchitectureBody (Ident ident) (NSimple (Ident name)   ) adecl astmt
      )

emptyState  :: String -> String -> ArchitectureState
emptyState ident name = Architecture ident (Entity name Nothing Nothing [] Nothing) [] []
  
behavioural :: String -> ArchitectureState
behavioural = emptyState "behavioural"

structural  :: String -> ArchitectureState
structural  = emptyState "structural"

--------------------------------------------------------------------------------
-- **

data Kind = Constant | Signal | Variable | File

type Type = SubtypeIndication

addDeclaration :: InterfaceDeclaration -> InterfaceList -> InterfaceList
addDeclaration new (InterfaceList is) = InterfaceList $ go new is
  where
    go :: InterfaceElement -> [InterfaceElement] -> [InterfaceElement]
    go d [] = [d]
    go d (i:is)
      | i `eqd` d = i `add` d : is
      | otherwise = i         : go d is
                    
    add :: InterfaceElement -> InterfaceElement -> InterfaceElement
    add old new = old { idecl_identifier_list = idecl_identifier_list old ++ [head $ idecl_identifier_list new] }

    eqd :: InterfaceDeclaration -> InterfaceDeclaration -> Bool
    eqd l r = l { idecl_identifier_list = [] } == r { idecl_identifier_list = [] }

newDeclaration :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> InterfaceDeclaration
newDeclaration ident kind mode typ exp = case kind of
  Constant -> InterfaceConstantDeclaration [ident]      typ       exp
  Signal   -> InterfaceSignalDeclaration   [ident] mode typ False exp
  Variable -> InterfaceVariableDeclaration [ident] mode typ       exp
  File     -> InterfaceFileDeclaration     [ident]      typ

addPort :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> LLVM Identifier
addPort ident kind mode typ exp =
  do state <- get
     let port  = newDeclaration ident kind mode typ exp
         ports = Just $ case entity_ports (architecture_header state) of
           Nothing              -> PortClause (InterfaceList [port])
           Just (PortClause is) -> PortClause (addDeclaration port is)
     modify (\s -> s { architecture_header = (architecture_header s) { entity_ports = ports } })
     return ident

addGeneric :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> LLVM Identifier
addGeneric ident kind mode typ exp =
  do state <- get
     let port  = newDeclaration ident kind mode typ exp
         ports = Just $ case entity_generics (architecture_header state) of
           Nothing                 -> GenericClause (InterfaceList [port])
           Just (GenericClause is) -> GenericClause (addDeclaration port is)
     modify (\s -> s { architecture_header = (architecture_header s) { entity_generics = ports } })
     return ident

--------------------------------------------------------------------------------
-- ***

constant, constantG :: String -> Type -> Maybe Expression -> LLVM Identifier
constant  str typ exp = addPort    (Ident str) Constant Nothing typ exp
constantG str typ exp = addGeneric (Ident str) Constant Nothing typ exp

signal, signalG :: String -> Mode -> Type -> Maybe Expression -> LLVM Identifier
signal  str mod typ exp = addPort    (Ident str) Signal (Just mod) typ exp
signalG str mod typ exp = addGeneric (Ident str) Signal (Just mod) typ exp

variable, variableG :: String -> Mode -> Type -> Maybe Expression -> LLVM Identifier
variable  str mod typ exp = addPort    (Ident str) Variable (Just mod) typ exp
variableG str mod typ exp = addGeneric (Ident str) Variable (Just mod) typ exp

file, fileG :: String -> Type -> LLVM Identifier
file  str typ = addPort    (Ident str) File Nothing typ Nothing >> return (Ident str)
fileG str typ = addGeneric (Ident str) File Nothing typ Nothing >> return (Ident str)

--------------------------------------------------------------------------------
-- * Declaring Entities
--------------------------------------------------------------------------------

addConcurrentStatement :: ConcurrentStatement -> LLVM ()
addConcurrentStatement cstmt = modify $ \s -> s {architecture_statements = architecture_statements s ++ [cstmt]}

--------------------------------------------------------------------------------

(<==) :: Identifier -> Expression -> LLVM ()
(<==) ident exp = addConcurrentStatement $
  ConSignalAss
    (CSASCond
      (Nothing)
      (False)
      (ConditionalSignalAssignment
        (TargetName (NSimple ident))
        (Options (False) (Nothing))
        (ConditionalWaveforms
          ([])
          ( (WaveElem [WaveEExp exp Nothing])
          , (Nothing)))))

--------------------------------------------------------------------------------
-- * Expressions - simplified for now
--------------------------------------------------------------------------------

relate :: [Identifier] -> [Relation]
relate = map
      ( (\x -> Relation x Nothing)
      . (\x -> ShiftExpression x Nothing)
      . (\x -> SimpleExpression Nothing x [])
      . (\x -> Term x [])
      . (\x -> FacPrim x Nothing)
      . PrimName
      . NSimple)

--------------------------------------------------------------------------------

and, or, xor, xnor :: [Identifier] -> Expression
and  = EAnd  . relate
or   = EOr   . relate
xor  = EXor  . relate
xnor = EXnor . relate

nand, nor :: [Identifier] -> Expression
nand [x,y] = let [a,b] = relate [x,y] in ENand a (Just b)
nor  [x,y] = let [a,b] = relate [x,y] in ENor  a (Just b)

not :: Expression -> Expression
not exp =
  (ENand
   (Relation
    (ShiftExpression
     (SimpleExpression
      (Nothing)
      (Term
       (FacNot
        (PrimExp
         (exp)))
       ([]))
      ([]))
     (Nothing))
    (Nothing))
   (Nothing))

--------------------------------------------------------------------------------
-- * Helpful instances and functions
--------------------------------------------------------------------------------

deriving instance Eq InterfaceDeclaration

deriving instance Eq Mode
  
deriving instance Eq SubtypeIndication

deriving instance Eq TypeMark
 
deriving instance Eq Identifier

instance Eq Expression -- todo
  where
    _ == _ = True
  
instance Eq Constraint  -- todo
  where
    _ == _ = False

instance Eq Name -- todo
  where
    NSimple n1 == NSimple n2 = n1 == n2
    _          == _          = False 
  
--------------------------------------------------------------------------------
-- * Test
--------------------------------------------------------------------------------

test :: IO ()
test =
  do let (head, body) = runLLVM (behavioural "even") $
           do a <- input "a"
              b <- input "b"
              c <- input "c"
              o <- output "o"
              o <== not (xor [a, b, c])
     putStrLn $ show $ pp head
     putStrLn $ show $ pp body

--------------------------------------------------------------------------------
-- **

input, output:: String -> LLVM Identifier
input  str = signal str In  std_logic Nothing
output str = signal str Out std_logic Nothing

inputs, outputs :: [String] -> LLVM [Identifier]
inputs  = mapM input
outputs = mapM output

std_logic :: Type
std_logic = SubtypeIndication Nothing (TMType (NSimple (Ident "STD_LOGIC"))) Nothing

--------------------------------------------------------------------------------
