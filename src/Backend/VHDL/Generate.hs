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
-- *
--------------------------------------------------------------------------------

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
-- * 
--------------------------------------------------------------------------------

addConcurrentStatement :: ConcurrentStatement -> LLVM ()
addConcurrentStatement cstmt = modify $ \s -> s {architecture_statements = architecture_statements s ++ [cstmt]}

addLocalDeclaration :: BlockDeclarativeItem -> LLVM ()
addLocalDeclaration bitem = modify $ \s -> s {architecture_declarative = architecture_declarative s ++ [bitem]}

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

localSignal :: Identifier -> Type -> LLVM ()
localSignal ident typ = addLocalDeclaration $
  BDISignalDecl
    (SignalDeclaration
      ([ident])
      (typ)
      (Nothing)
      (Nothing)
    )

--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

class Dummy a where
  relate :: a -> Relation

instance Dummy Relation where
  relate = id

instance Dummy ShiftExpression where
  relate = flip Relation Nothing

instance Dummy SimpleExpression where
  relate = relate . flip ShiftExpression Nothing

instance Dummy Term where
  relate = relate . flip (SimpleExpression Nothing) []

instance Dummy Factor where
  relate = relate . flip Term []

instance Dummy Primary where
  relate = relate . flip FacPrim Nothing

instance Dummy Identifier
  where
    relate = relate . PrimName . NSimple

instance Dummy Expression
  where
    relate = relate . PrimExp

dummy :: Dummy a => a -> Expression
dummy = flip ENand Nothing . relate

--------------------------------------------------------------------------------
-- **

and, or, xor, xnor :: Dummy a => [a] -> Expression
and  = EAnd  . map relate
or   = EOr   . map relate
xor  = EXor  . map relate
xnor = EXnor . map relate

nand, nor :: Dummy a => [a] -> Expression
nand [x,y] = ENand (relate x) (Just $ relate y)
nor  [x,y] = ENor  (relate x) (Just $ relate y)

not :: Dummy a => a -> Expression
not exp =
  (ENand
   (Relation
    (ShiftExpression
     (SimpleExpression
      (Nothing)
      (Term
       (FacNot
        (PrimExp
         (dummy exp)))
       ([]))
      ([]))
     (Nothing))
    (Nothing))
   (Nothing))

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

std_logic :: Type
std_logic = SubtypeIndication Nothing (TMType (NSimple (Ident "STD_LOGIC"))) Nothing

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
