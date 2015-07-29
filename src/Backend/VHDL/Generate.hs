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

addDeclaration :: InterfaceDeclaration -> InterfaceList -> InterfaceList
addDeclaration new (InterfaceList is) = InterfaceList $ go new is
  where
    go :: InterfaceElement -> [InterfaceElement] -> [InterfaceElement]
    go d [] = [d]
    go d (i:is)
      | i `eqd` d = i `add` d : is
      | otherwise = i         : go d is
                    
    add :: InterfaceElement -> InterfaceElement -> InterfaceElement
    add old new = old { idecl_identifier_list = (head $ idecl_identifier_list new) : idecl_identifier_list old }

    eqd :: InterfaceDeclaration -> InterfaceDeclaration -> Bool
    eqd l r = l { idecl_identifier_list = [] } == r { idecl_identifier_list = [] }


newDeclaration :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> InterfaceDeclaration
newDeclaration ident kind mode typ exp = case kind of
  Constant -> InterfaceConstantDeclaration [ident]      typ       exp
  Signal   -> InterfaceSignalDeclaration   [ident] mode typ False exp
  Variable -> InterfaceVariableDeclaration [ident] mode typ       exp
  File     -> InterfaceFileDeclaration     [ident]      typ

addPort :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> LLVM ()
addPort ident kind mode typ exp =
  do state <- get
     let port  = newDeclaration ident kind mode typ exp
         ports = Just $ case entity_ports (architecture_header state) of
           Nothing              -> PortClause (InterfaceList [port])
           Just (PortClause is) -> PortClause (addDeclaration port is)
{-
addPort ident kind mode typ exp ed@(EntityDeclaration _ eh@(EntityHeader _ ports) _ _) = undefined
  let port = newDeclaration ident kind mode typ exp
      new  = case ports of
        Nothing              -> Just (PortClause (InterfaceList [port]))
        Just (PortClause is) -> Just (PortClause (addDeclaration port is))
   in ed { entity_header = eh { formal_port_clause = new } }
-}
addGeneric :: Identifier -> Kind -> Maybe Mode -> Type -> Maybe Expression -> LLVM ()
addGeneric ident kind mode typ exp {-ed@(EntityDeclaration _ eh@(EntityHeader gens _) _ _)-} = undefined
{-
  let gen = newDeclaration ident kind mode typ exp
      new = case gens of
        Nothing                 -> Just (GenericClause (InterfaceList [gen]))
        Just (GenericClause is) -> Just (GenericClause (addDeclaration gen is))
   in ed { entity_header = eh { formal_generic_clause = new } }
-}
--------------------------------------------------------------------------------
-- ***
{-
constant, constantG :: String -> Type -> Maybe Expression -> EntityDeclaration -> EntityDeclaration
constant  str typ exp = addPort    (Ident str) Constant Nothing typ exp
constantG str typ exp = addGeneric (Ident str) Constant Nothing typ exp

signal, signalG :: String -> Mode -> Type -> Maybe Expression -> EntityDeclaration -> EntityDeclaration
signal  str mod typ exp = addPort    (Ident str) Signal (Just mod) typ exp
signalG str mod typ exp = addGeneric (Ident str) Signal (Just mod) typ exp

variable, variableG :: String -> Mode -> Type -> Maybe Expression -> EntityDeclaration -> EntityDeclaration
variable  str mod typ exp = addPort    (Ident str) Variable (Just mod) typ exp
variableG str mod typ exp = addGeneric (Ident str) Variable (Just mod) typ exp

file, fileG :: String -> Type -> EntityDeclaration -> EntityDeclaration
file  str typ = addPort    (Ident str) File Nothing typ Nothing
fileG str typ = addGeneric (Ident str) File Nothing typ Nothing
-}
--------------------------------------------------------------------------------
-- * Declaring Entities
--------------------------------------------------------------------------------

data Kind = Constant | Signal | Variable | File

type Type = SubtypeIndication

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

addConcurrentStatement :: ConcurrentStatement -> ArchitectureBody -> ArchitectureBody
addConcurrentStatement cs (ArchitectureBody i n dp sp) = (ArchitectureBody i n dp (sp ++ [cs]))

--------------------------------------------------------------------------------

conditionalAssignment :: Maybe Label -> Bool -> ConditionalSignalAssignment -> ArchitectureBody -> ArchitectureBody 
conditionalAssignment label post csas = undefined

selectedAssignment :: Maybe Label -> Bool -> SelectedSignalAssignment -> ArchitectureBody -> ArchitectureBody
selectedAssignment label post ssas = undefined

--------------------------------------------------------------------------------

(<==) :: Identifier -> Expression -> ArchitectureBody -> ArchitectureBody
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
          ( (WaveElem
              [WaveEExp
                (exp)
                (Nothing)
              ]
            )
          , (Nothing)
          )
        )
      )
    )

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
{-
testD = (newInputs ["a", "b", "c"] >> newOutput "o")                $ emptyEntity "even"
testB = (Ident "o") <== not (xor [Ident "a", Ident "b", Ident "c"]) $ behavioural "even"
-}
--------------------------------------------------------------------------------
-- **
{-
newInput, newOutput :: String -> EntityDeclaration -> EntityDeclaration
newInput  str = signal str In  std_logic Nothing
newOutput str = signal str Out std_logic Nothing

newInputs, newOutputs :: [String] -> EntityDeclaration -> EntityDeclaration
newInputs  = flip $ foldr newInput
newOutputs = flip $ foldr newOutput
-}
std_logic :: Type
std_logic = SubtypeIndication Nothing (TMType (NSimple (Ident "STD_LOGIC"))) Nothing

--------------------------------------------------------------------------------
