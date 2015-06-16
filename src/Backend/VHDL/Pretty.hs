{-# LANGUAGE StandaloneDeriving #-}

module Backend.VHDL.Pretty where

import Backend.VHDL.Syntax

import Text.PrettyPrint hiding (Mode)

--------------------------------------------------------------------------------
-- * Pretty printing class
--------------------------------------------------------------------------------

class Pretty a
  where
    pp :: a -> Doc

--------------------------------------------------------------------------------

instance Pretty a => Pretty [a]
  where
    pp = hsep . map pp

--------------------------------------------------------------------------------
-- * Pretty printing instances
--------------------------------------------------------------------------------

instance Pretty AbstractLiteral where pp = undefined

instance Pretty AccessTypeDefinition where
  pp (AccessTypeDefinition s) = text "ACCESS" <+> pp s

instance Pretty ActualDesignator where
  pp (ADExpression e) = pp e
  pp (ADSignal n)     = pp n
  pp (ADVariable n)   = pp n
  pp (ADFile n)       = pp n
  pp (ADOpen)         = text "OPEN"

--instance Pretty ActualParameterPart where pp = undefined

instance Pretty ActualPart where
  pp (APDesignator a) = pp a
  pp (APFunction f a) = pp f <+> parens (pp a)
  pp (APType t a)     = pp t <+> parens (pp a)

instance Pretty AddingOperator where
  pp (Plus)   = char '+'
  pp (Minus)  = char '-'
  pp (Concat) = char '&'

instance Pretty Aggregate where
  pp (Aggregate es) = parens (commaSep $ map pp es)

instance Pretty AliasDeclaration where
  pp (AliasDeclaration a sub n sig) =
        text "ALIAS" <+> pp a
    <+> cond (colon <+>) sub
    <+> text "IS" <+> pp n
    <+> cond id sig <+> semi

instance Pretty AliasDesignator where
  pp (ADIdentifier i) = pp i
  pp (ADCharacter c)  = pp c
  pp (ADOperator o)   = pp o

instance Pretty Allocator where
  pp (AllocSub s)  = text "NEW" <+> pp s
  pp (AllocQual q) = text "NEW" <+> pp q

instance Pretty ArchitectureBody where
  pp (ArchitectureBody i n d s) =
      vcat [header, indent (pp d), text "BEGIN", indent (pp s), footer]
    where
      header = text "ARCHITECTURE" <+> pp i
           <+> text "OF" <+> pp n
           <+> text "IS"
      footer = text "END ARCITECTURE" <+> pp n <+> semi

--instance Pretty ArchitectureDeclarativePart where pp = undefined

--instance Pretty ArchitectureStatementPart where pp = undefined

instance Pretty ArrayTypeDefinition where
  pp (ArrU u) = pp u
  pp (ArrC c) = pp c

instance Pretty Assertion where
  pp (Assertion c r s) = vcat [text "ASSERT" <+> pp c, report, severity]
    where
      report   = indent $ cond (text "REPORT" <+>) r
      severity = indent $ cond (text "SEVERITY" <+>) s

instance Pretty AssertionStatement where
  pp (AssertionStatement l a) = label l <+> pp a <+> semi

instance Pretty AssociationElement where
  pp (AssociationElement f a) = condR (text "=>") f <+> pp a

instance Pretty AssociationList where
  pp (AssociationList as) = commaSep $ map pp as

instance Pretty AttributeDeclaration where
  pp (AttributeDeclaration i t) = text "ATTRIBUTE" <+> pp i <+> colon <+> pp t <+> semi

--instance Pretty AttributeDesignator where pp = undefined

instance Pretty AttributeName where
  pp (AttributeName p s d e) = pp p <+> cond id s <+> char '\'' <+> pp d <+> cond parens e

instance Pretty AttributeSpecification where
  pp (AttributeSpecification d s e) =
        text "ATTRIBUTE" <+> pp d
    <+> text "OF" <+> pp s
    <+> text "IS" <+> pp e <+> semi

instance Pretty Base where pp = undefined -- todo

instance Pretty BaseSpecifier where pp = undefined -- todo

instance Pretty BaseUnitDeclaration where pp = undefined -- todo

instance Pretty BasedInteger where pp = undefined -- todo

instance Pretty BasedLiteral where pp = undefined -- todo

instance Pretty BasicCharacter where pp = undefined -- todo

instance Pretty BasicGraphicCharacter where pp = undefined -- todo

instance Pretty BasicIdentifier where pp = undefined -- todo

instance Pretty BindingIndication where
  pp (BindingIndication e g p) =
    vcat [condR (text "USE") e, cond id g, cond id p]

instance Pretty BitStringLiteral where pp = undefined -- todo

instance Pretty BitValue where pp = undefined -- todo

instance Pretty BlockConfiguration where
  pp (BlockConfiguration s u c) =
    vcat [ text "FOR" <+> pp s
         , indent (pp u)
         , indent (pp c)
         , text "END FOR" <+> semi]

instance Pretty BlockDeclarativeItem where
  pp (BDISubprogDecl d)  = pp d
  pp (BDISubprogBody b)  = pp b
  pp (BDITypeDecl t)     = pp t
  pp (BDISubtypeDecl s)  = pp s
  pp (BDIConstantDecl c) = pp c
  pp (BDISignalDecl s)   = pp s
  pp (BDISharedDecl v)   = pp v
  pp (BDIFileDecl f)     = pp f
  pp (BDIAliasDecl a)    = pp a
  pp (BDICompDecl c)     = pp c
  pp (BDIAttrDecl a)     = pp a
  pp (BDIAttrSepc a)     = pp a
  pp (BDIConfigSepc c)   = pp c
  pp (BDIDisconSpec d)   = pp d
  pp (BDIUseClause u)    = pp u
  pp (BDIGroupTemp g)    = pp g
  pp (BDIGroupDecl g)    = pp g

--instance Pretty BlockDeclarativePart where pp = undefined

instance Pretty BlockHeader where
  pp (BlockHeader p g) =
      vcat [go p, go g]
    where
      go :: (Pretty a, Pretty b) => Maybe (a, Maybe b) -> Doc
      go (Nothing)      = empty
      go (Just (a, mb)) = pp a $+$ cond indent mb

instance Pretty BlockSpecification where
  pp (BSArch n)  = pp n
  pp (BSBlock l) = pp l
  pp (BSGen l)   = pp l

instance Pretty BlockStatement where
  pp (BlockStatement l g h d s) =
      pp l <+> colon `hangs` vcat [header, body, footer]
    where
      header = text "BLOCK" <+> cond parens g <+> text "IS" `hangs` (pp h $$ pp d)
      body   = text "BEGIN" `hangs` (pp s)
      footer = text "END BLOCK" <+> pp l

--instance Pretty BlockStatementPart where pp = undefined

instance Pretty CaseStatement where
  pp (CaseStatement l e cs) =
      cond id l <+> colon `hangs` vcat [header, body, footer]
    where
      header = text "CASE" <+> pp e <+> text "IS"
      body   = indent $ vcat $ map pp cs
      footer = text "END CASE" <+> cond id l

instance Pretty CaseStatementAlternative where
  pp (CaseStatementAlternative c ss) =
    text "WHEN" <+> pp c <+> text "=>" `hangs` pp ss

instance Pretty CharacterLiteral where
  pp (CLit c) = char c

instance Pretty Choice where
  pp (ChoiceSimple s) = pp s
  pp (ChoiceRange r)  = pp r
  pp (ChoiceName n)   = pp n
  pp (ChoiceOthers)   = text "OTHERS"

instance Pretty Choices where
  pp (Choices cs) = pipeSep $ map pp cs

instance Pretty ComponentConfiguration where
  pp (ComponentConfiguration s i c) =
    vcat [ text "FOR" <+> pp s
         , indent $ vcat
           [ condR semi i
           , cond  id c
           ]
         , text "END FOR" <+> semi
         ]

instance Pretty ComponentDeclaration where
  pp (ComponentDeclaration i g p s) =
    vcat [ text "COMPONENT" <+> pp i <+> text "IS"
         , indent $ vcat
           [ cond id g
           , cond id p
           ]
         , text "END COMPONENT" <+> cond id s <+> semi
         ]

instance Pretty ComponentInstantiationStatement where
  pp (ComponentInstantiationStatement l u g p) =
    pp l <+> colon `hangs` (pp u `hangs` vcat [cond id g, cond id p])

instance Pretty ComponentSpecification where
  pp (ComponentSpecification ls n) = pp ls <+> colon <+> pp n

instance Pretty CompositeTypeDefinition where
  pp (CTDArray at)  = pp at
  pp (CTDRecord rt) = pp rt

instance Pretty ConcurrentAssertionStatement where
  pp (ConcurrentAssertionStatement l p a) = postponed l p a

instance Pretty ConcurrentProcedureCallStatement where
  pp (ConcurrentProcedureCallStatement l p a) = postponed l p a

instance Pretty ConcurrentSignalAssignmentStatement where
  pp (CSASCond l p a)   = postponed l p a
  pp (CSASSelect l p a) = postponed l p a

instance Pretty ConcurrentStatement where
  pp (ConBlock b)     = pp b
  pp (ConProcess p)   = pp p
  pp (ConProcCall c)  = pp c
  pp (ConAssertion a) = pp a
  pp (ConSignalAss s) = pp s
  pp (ConComponent c) = pp c
  pp (ConGenerate g)  = pp g

--instance Pretty Condition where pp = undefined

instance Pretty ConditionClause where
  pp (ConditionClause e) = text "UNTIL" <+> pp e

instance Pretty ConditionalSignalAssignment where
  pp (ConditionalSignalAssignment t o w) = pp t <+> text "<=" <+> pp o <+> pp w <+> semi

instance Pretty ConditionalWaveforms where
  pp (ConditionalWaveforms ws (w, c)) =
      vcat ws' $$ pp w <+> condL (text "WHEN") c
    where
      ws' = map (\(w, c) -> pp w <+> text "WHEN" <+> pp c <+> text "ELSE") ws
  
instance Pretty ConfigurationDeclaration where
  pp (ConfigurationDeclaration i n d b) =
    vcat [ text "CONFIGURATION" <+> pp i <+> text "OF" <+> pp n <+> text "IS"
         , indent $ vcat
           [ pp d
           , pp b
           ]
         , text "END CONFIGURATION" <+> pp i
         ]

instance Pretty ConfigurationDeclarativeItem where
  pp (CDIUse u)   = pp u
  pp (CDIAttr a)  = pp a
  pp (CDIGroup g) = pp g

--instance Pretty ConfigurationDeclarativePart where pp = undefined

instance Pretty ConfigurationItem where
  pp (CIBlock b) = pp b
  pp (CIComp c)  = pp c

instance Pretty ConfigurationSpecification where
  pp (ConfigurationSpecification s i) = text "FOR" <+> pp s <+> pp i <+> semi

instance Pretty ConstantDeclaration where
  pp (ConstantDeclaration is s e) =
    text "CONSTANT" <+> pp is <+> colon <+> pp s <+> condL (text ":=") e

instance Pretty ConstrainedArrayDefinition where
  pp (ConstrainedArrayDefinition i s) = text "ARRAY" <+> pp i <+> text "OF" <+> pp s

instance Pretty Constraint where
  pp (CRange r) = pp r
  pp (CIndex i) = pp i

instance Pretty ContextClause where pp = undefined -- todo

instance Pretty ContextItem where pp = undefined -- todo

instance Pretty DecimalLiteral where pp = undefined -- todo

instance Pretty Declaration where
  pp (DType t)          = pp t
  pp (DSubtype s)       = pp s
  pp (DObject o)        = pp o
  pp (DAlias a)         = pp a
  pp (DComponent c)     = pp c
  pp (DAttribute a)     = pp a
  pp (DGroupTemplate g) = pp g
  pp (DGroup g)         = pp g
  pp (DEntity e)        = pp e
  pp (DConfiguration c) = pp c
  pp (DSubprogram s)    = pp s
  pp (DPackage p)       = pp p

instance Pretty DelayMechanism where
  pp (DMechTransport)  = text "TRANSPORT"
  pp (DMechInertial e) = condL (text "REJECT") e <+> text "INERTIAL"

instance Pretty DesignFile where pp = undefined -- todo

instance Pretty DesignUnit where pp = undefined -- todo

instance Pretty Designator where
  pp (DId i) = pp i
  pp (DOp o) = pp o

instance Pretty Direction where
  pp (To)     = text "TO"
  pp (DownTo) = text "DOWNTO"

instance Pretty DisconnectionSpecification where
  pp (DisconnectionSpecification g e) =
    text "DISCONNECT" <+> pp g <+> text "AFTER" <+> pp e <+> semi

instance Pretty DiscreteRange where
  pp (DRSub s)   = pp s
  pp (DRRange r) = pp r

instance Pretty ElementAssociation where
  pp (ElementAssociation c e) = condR (text "=>") c <+> pp e

instance Pretty ElementDeclaration where
  pp (ElementDeclaration is s) = pp is <+> colon <+> pp s <+> semi

--instance Pretty ElementSubtypeDefinition where pp = undefined

instance Pretty EntityAspect where
  pp (EAEntity n i) = text "ENTITY" <+> pp n <+> cond parens i
  pp (EAConfig n)   = text "CONFIGURATION" <+> pp n
  pp (EAOpen)       = text "OPEN"

instance Pretty EntityClass where
  pp ENTITY        = text "ENTITY"
  pp ARCHITECTURE  = text "ARCHITECTURE"
  pp CONFIGURATION = text "CONFIGURATION"
  pp PROCEDURE     = text "PROCEDURE"
  pp FUNCTION      = text "FUNCTION"
  pp PACKAGE       = text "PACKAGE"
  pp TYPE          = text "TYPE"
  pp SUBTYPE       = text "SUBTYPE"
  pp CONSTANT      = text "CONSTANT"
  pp SIGNAL        = text "SIGNAL"
  pp VARIABLE      = text "VARIABLE"
  pp COMPONENT     = text "COMPONENT"
  pp LABEL         = text "LABEL"
  pp LITERAL       = text "LITERAL"
  pp UNITS         = text "UNITS"
  pp GROUP         = text "GROUP"
  pp FILE          = text "FILE"

instance Pretty EntityClassEntry where
  pp (EntityClassEntry c m) = pp c <+> when m (text "<>")

--instance Pretty EntityClassEntryList where pp = undefined

instance Pretty EntityDeclaration where
  pp (EntityDeclaration i h d s) =
    vcat [ text "ENTITY" <+> pp i <+> text "IS"
         , indent $ vcat
           [ pp h
           , pp d
           ]
         , cond (flip hangs (text "BEGIN")) s
         , text "END ENTITY" <+> pp i <+> semi
         ]

instance Pretty EntityDeclarativeItem where
  pp (EDISubprogDecl s)  = pp s
  pp (EDISubprogBody b)  = pp b
  pp (EDITypeDecl t)     = pp t
  pp (EDISubtypeDecl s)  = pp s
  pp (EDIConstantDecl c) = pp c
  pp (EDISignalDecl s)   = pp s
  pp (EDISharedDecl s)   = pp s
  pp (EDIFileDecl f)     = pp f
  pp (EDIAliasDecl a)    = pp a
  pp (EDIAttrDecl a)     = pp a
  pp (EDIAttrSpec a)     = pp a
  pp (EDIDiscSpec d)     = pp d
  pp (EDIUseClause u)    = pp u
  pp (EDIGroupTemp g)    = pp g
  pp (EDIGroupDecl g)    = pp g

--instance Pretty EntityDeclarativePart where pp = undefined

instance Pretty EntityDesignator where
  pp (EntityDesignator t s) = pp t <+> cond id s

instance Pretty EntityHeader where
  pp (EntityHeader g p) = vcat [cond indent g, cond indent p]

instance Pretty EntityNameList where
  pp (ENLDesignators es) = commaSep $ fmap pp es

instance Pretty EntitySpecification where
  pp (EntitySpecification ns c) = pp ns <+> colon <+> pp c

instance Pretty EntityStatement where
  pp (ESConcAssert a)  = pp a
  pp (ESPassiveConc p) = pp p
  pp (ESPassiveProc p) = pp p

--instance Pretty EntityStatementPart where pp = undefined

instance Pretty EntityTag where
  pp (ETName n) = pp n
  pp (ETChar c) = pp c
  pp (ETOp o)   = pp o

instance Pretty EnumerationLiteral where
  pp (EId i)   = pp i
  pp (EChar c) = pp c

instance Pretty EnumerationTypeDefinition where
  pp (EnumerationTypeDefinition es) = commaSep $ fmap pp es

instance Pretty ExitStatement where
  pp (ExitStatement l b c) =
        condR colon l
    <+> text "NEXT" <+> cond id b
    <+> cond ((<+>) (text "WHEN")) c <+> semi

instance Pretty Exponent where pp = undefined -- todo

instance Pretty Expression where
  pp (EAnd rs)  = pp rs
  pp (EOr rs)   = pp rs
  pp (EXor rs)  = pp rs
  pp (ENAnd rs) = cond id rs
  pp (ENor rs)  = cond id rs
  pp (EXNor rs) = pp rs

instance Pretty ExtendedDigit where pp = undefined -- todo

instance Pretty ExtendedIdentifier where pp = undefined -- todo

instance Pretty Factor where
  pp (FacPrim p mp) = pp p <+> condL (text "**") mp
  pp (FacAbs p)     = text "ABS" <+> pp p
  pp (FacNot p)     = text "NOT" <+> pp p

instance Pretty FileDeclaration where
  pp (FileDeclaration is s o) = text "FILE" <+> pp is <+> colon <+> pp s <+> cond id o <+> semi

--instance Pretty FileLogicalName where pp = undefined

instance Pretty FileOpenInformation where
  pp (FileOpenInformation e n) = condL (text "OPEN") e <+> text "IS" <+> pp n

instance Pretty FileTypeDefinition where
  pp (FileTypeDefinition t) = text "FILE OF" <+> pp t

--instance Pretty FloatingTypeDefinition where pp = undefined

instance Pretty FormalDesignator where
  pp (FDGeneric n)   = pp n
  pp (FDPort n)      = pp n
  pp (FDParameter n) = pp n

--instance Pretty FormalParameterList where pp = undefined

instance Pretty FormalPart where
  pp (FPDesignator d) = pp d
  pp (FPFunction n d) = pp n <+> parens (pp d)
  pp (FPType t d)     = pp t <+> parens (pp d)

instance Pretty FullTypeDeclaration where
  pp (FullTypeDeclaration i t) = text "TYPE" <+> pp i <+> text "IS" <+> pp t <+> semi

instance Pretty FunctionCall where
  pp (FunctionCall n p) = pp n <+> cond parens p

instance Pretty GenerateStatement where
  pp (GenerateStatement l g d s) =
    pp l <+> colon `hangs` vcat
      [ pp g <+> text "GENERATE"
      , cond indent d
      , cond (const $ text "BEGIN") d
      , indent $ vcat $ fmap pp s
      , text "END GENERATE" <+> pp l <+> semi
      ]

instance Pretty GenerationScheme where
  pp (GSFor p) = pp p
  pp (GSIf c)  = pp c

instance Pretty GenericClause where
  pp (GenericClause ls) = text "GENERIC" <+> parens (pp ls)

--instance Pretty GenericList where pp = undefined

instance Pretty GenericMapAspect where
  pp (GenericMapAspect as) = text "GENERIC MAP" <+> parens (pp as)

instance Pretty GraphicCharacter where pp = undefined -- todo

instance Pretty GroupConstituent where
  pp (GCName n) = pp n
  pp (GCChar c) = pp c

--instance Pretty GroupConstituentList where pp = undefined

instance Pretty GroupTemplateDeclaration where
  pp (GroupTemplateDeclaration i cs) = text "GROUP" <+> pp i <+> text "IS" <+> parens (pp cs) <+> semi

instance Pretty GroupDeclaration where
  pp (GroupDeclaration i n cs) = text "GROUP" <+> pp i <+> colon <+> pp n <+> parens (pp cs) <+> semi

instance Pretty GuardedSignalSpecification where
  pp (GuardedSignalSpecification ss t) = pp ss <+> colon <+> pp t

instance Pretty Identifier where
  pp (Ident i) = text i

--instance Pretty IdentifierList where pp = undefined

instance Pretty IfStatement where
  pp (IfStatement l (tc, ts) a e) =
    condR colon l `hangs` vcat
      [ text "IF" <+> pp tc <+> text "THEN"
      , vcat $ fmap (\(c, s) -> text "ELSEIF" <+> pp c <+> text "THEN" `hangs` pp s) a
      , cond (hangs (text "ELSE")) e
      , text "END IF" <+> cond id l <+> semi
      ]

instance Pretty IncompleteTypeDeclaration where
  pp (IncompleteTypeDeclaration i) = text "TYPE" <+> pp i <+> semi

instance Pretty IndexConstraint where
  pp (IndexConstraint rs) = parens (commaSep $ map pp rs)

instance Pretty IndexSpecification where
  pp (ISRange r) = pp r
  pp (ISExp e)   = pp e

instance Pretty IndexSubtypeDefinition where
  pp (IndexSubtypeDefinition t) = pp t <+> text "RANGE" <+> semi

instance Pretty IndexedName where
  pp (IndexedName p es) = pp p <+> parens (commaSep $ map pp es)

instance Pretty InstantiatedUnit where
  pp (IUComponent n) = text "COMPONENT" <+> pp n
  pp (IUEntity n i)  = text "ENTITY" <+> pp n <+> cond parens i
  pp (IUConfig n)    = text "CONFIGURATION" <+> pp n

instance Pretty InstantiationList where
  pp (ILLabels ls) = commaSep $ map pp ls
  pp (ILOthers)    = text "OTHERS"
  pp (ILAll)       = text "ALL"

instance Pretty Integer where pp = integer

--instance Pretty IntegerTypeDefinition where pp = undefined

instance Pretty InterfaceDeclaration where
  pp (InterfaceConstantDeclaration is s e) =
    text "CONSTANT" <+> pp is <+> colon <+> text "IN" <+> pp s <+> condL (text ":=") e
  pp (InterfaceSignalDeclaration is m s b e) =
    text "SIGNAL" <+> pp is <+> colon <+> cond id m <+> pp s <+> when b (text "BUS") <+> condL (text ":=") e
  pp (InterfaceVariableDeclaration is m s e) =
    text "VARIABLE" <+> pp is <+> colon <+> cond id m <+> pp s <+> condL (text ":=") e
  pp (InterfaceFileDeclaration is s) =
    text "FILE" <+> pp is <+> colon <+> pp s

--instance Pretty InterfaceElement where pp = undefined

instance Pretty InterfaceList where
  pp (InterfaceList es) = semiSep $ map pp es

instance Pretty IterationScheme where
  pp (IterWhile c) = text "WHILE" <+> pp c
  pp (IterFor p)   = text "FOR" <+> pp p

--instance Pretty Label where pp = undefined

instance Pretty Letter where pp = undefined -- todo

instance Pretty LetterOrDigit where pp = undefined -- todo

instance Pretty LibraryClause where pp = undefined -- todo

instance Pretty LibraryUnit where pp = undefined -- todo

instance Pretty Literal where
  pp (LitNum n)       = pp n
  pp (LitEnum e)      = pp e
  pp (LitString s)    = pp s
  pp (LitBitString b) = pp b
  pp (LitNull)        = text "NULL"

instance Pretty LogicalName where pp = undefined -- todo

instance Pretty LogicalNameList where pp = undefined -- todo

instance Pretty LogicalOperator where
  pp (And)  = text "AND"
  pp (Or)   = text "OR"
  pp (Nand) = text "NAND"
  pp (Nor)  = text "NOR"
  pp (Xor)  = text "XOR"
  pp (Xnor) = text "XNOR"

instance Pretty LoopStatement where
  pp (LoopStatement l i ss) =
    condR colon l `hangs` vcat
      [ cond id i <+> text "LOOP"
      , indent $ pp ss
      , text "END LOOP" <+> cond id l <+> semi
      ]

instance Pretty MiscellaneousOperator where
  pp (Exp) = text "**"
  pp (Abs) = text "ABS"
  pp (Not) = text "NOT"

instance Pretty Mode where
  pp (In)      = text "IN"
  pp (Out)     = text "OUT"
  pp (InOut)   = text "INOUT"
  pp (Buffer)  = text "BUFFER"
  pp (Linkage) = text "LINKAGE"

instance Pretty MultiplyingOperator where
  pp (Times) = char '*'
  pp (Div)   = char '/'
  pp (Mod)   = text "MOD"
  pp (Rem)   = text "REM"

instance Pretty Name where
  pp (NSimple n) = pp n
  pp (NOp o)     = pp o
  pp (NSelect s) = pp s
  pp (NIndex i)  = pp i
  pp (NSlice s)  = pp s
  pp (NAttr a)   = pp a

instance Pretty NextStatement where
  pp (NextStatement l b c) = condR colon l <+> text "NEXT" <+> cond id b <+> condL (text "WHEN") c <+> semi

instance Pretty NullStatement where
  pp (NullStatement l) = condR colon l <+> text "NULL"

instance Pretty NumericLiteral where
  pp (NLitAbstract a) = pp a
  pp (NLitPhysical p) = pp p

instance Pretty ObjectDeclaration where
  pp (ObjConst c) = pp c
  pp (ObjSig s)   = pp s
  pp (ObjVar v)   = pp v
  pp (ObjFile f)  = pp f

--instance Pretty OperatorSymbol where pp = undefined

instance Pretty Options where
  pp (Options g d) = when g (text "GUARDED") <+> cond id d

instance Pretty PackageBody where
  pp (PackageBody n d) =
    vcat [ text "PACKAGE BODY" <+> pp n <+> text "IS"
         , indent $ pp d
         , text "END PACKAGE BODY" <+> pp n <+> semi
         ]

instance Pretty PackageBodyDeclarativeItem where
  pp (PBDISubprogDecl s)  = pp s
  pp (PBDISubprogBody b)  = pp b
  pp (PBDITypeDecl t)     = pp t
  pp (PBDISubtypeDecl s)  = pp s
  pp (PBDIConstantDecl c) = pp c
  pp (PBDISharedDecl s)   = pp s
  pp (PBDIFileDecl f)     = pp f
  pp (PBDIAliasDecl a)    = pp a
  pp (PBDIUseClause u)    = pp u
  pp (PBDIGroupTemp g)    = pp g
  pp (PBDIGroupDecl g)    = pp g

--Instance Pretty PackageBodyDeclarativePart where pp = undefined

instance Pretty PackageDeclaration where
  pp (PackageDeclaration i d) =
    vcat [ text "PACKAGE" <+> pp i <+> text "IS"
         , indent $ pp d
         , text "END PACKAGE" <+> pp i <+> semi
         ]

instance Pretty PackageDeclarativeItem where
  pp (PDISubprogDecl s)  = pp s
  pp (PDISubprogBody b)  = pp b
  pp (PDITypeDecl t)     = pp t
  pp (PDISubtypeDecl s)  = pp s
  pp (PDIConstantDecl c) = pp c
  pp (PDISignalDecl s)   = pp s
  pp (PDISharedDecl v)   = pp v
  pp (PDIFileDecl f)     = pp f
  pp (PDIAliasDecl a)    = pp a
  pp (PDICompDecl c)     = pp c
  pp (PDIAttrDecl a)     = pp a
  pp (PDIAttrSpec a)     = pp a
  pp (PDIDiscSpec d)     = pp d
  pp (PDIUseClause u)    = pp u
  pp (PDIGroupTemp g)    = pp g
  pp (PDIGroupDecl g)    = pp g
  
--instance Pretty PackageDeclarativePart where pp = undefined

instance Pretty ParameterSpecification where
  pp (ParameterSpecification i r) = pp i <+> text "IN" <+> pp r

instance Pretty PhysicalLiteral where
  pp (PhysicalLiteral a n) = cond id a <+> pp n

instance Pretty PhysicalTypeDefinition where
  pp (PhysicalTypeDefinition c p s n) =
    pp c `hangs` vcat
      [ text "UNITS"
      , indent $ vcat
        [ pp p
        , vcat $ map pp s
        ]
      , text "END UNITS" <+> cond id n
      ]

instance Pretty PortClause where pp = undefined

--instance Pretty PortList where pp = undefined

instance Pretty PortMapAspect where pp = undefined

instance Pretty Prefix where pp = undefined

instance Pretty Primary where pp = undefined

instance Pretty PrimaryUnit where pp = undefined

instance Pretty ProcedureCall where pp = undefined

instance Pretty ProcedureCallStatement where pp = undefined

instance Pretty ProcessDeclarativeItem where pp = undefined

--instance Pretty ProcessDeclarativePart where pp = undefined

instance Pretty ProcessStatement where pp = undefined

--instance Pretty ProcessStatementPart where pp = undefined

instance Pretty QualifiedExpression where pp = undefined

instance Pretty Range where pp = undefined

instance Pretty RangeConstraint where pp = undefined

instance Pretty RecordTypeDefinition where pp = undefined

instance Pretty Relation where pp = undefined

instance Pretty RelationalOperator where pp = undefined

instance Pretty ReportStatement where pp = undefined

instance Pretty ReturnStatement where pp = undefined

instance Pretty ScalarTypeDefinition where pp = undefined

instance Pretty SecondaryUnit where pp = undefined

instance Pretty SecondaryUnitDeclaration where pp = undefined

instance Pretty SelectedName where pp = undefined

instance Pretty SelectedSignalAssignment where pp = undefined

instance Pretty SelectedWaveforms where pp = undefined

instance Pretty SensitivityClause where pp = undefined

instance Pretty SensitivityList where pp = undefined

--instance Pretty SequenceOfStatements where pp = undefined

instance Pretty SequentialStatement where pp = undefined

instance Pretty ShiftExpression where pp = undefined

instance Pretty ShiftOperator where pp = undefined

instance Pretty Sign where pp = undefined

instance Pretty SignalAssignmentStatement where pp = undefined

instance Pretty SignalDeclaration where pp = undefined

instance Pretty SignalKind where pp = undefined

instance Pretty SignalList where pp = undefined

instance Pretty Signature where pp = undefined

instance Pretty SimpleExpression where pp = undefined

--instance Pretty SimpleName where pp = undefined

instance Pretty SliceName where pp = undefined

instance Pretty StringLiteral where pp = undefined

instance Pretty SubprogramBody where pp = undefined

--instance Pretty SubprogramDeclaration where pp = undefined

instance Pretty SubprogramDeclarativeItem where pp = undefined

--instance Pretty SubprogramDeclarativePart where pp = undefined

instance Pretty SubprogramKind where pp = undefined

instance Pretty SubprogramSpecification where pp = undefined

--instance Pretty SubprogramStatementPart where pp = undefined

instance Pretty SubtypeDeclaration where pp = undefined

instance Pretty SubtypeIndication where pp = undefined

instance Pretty Suffix where pp = undefined

instance Pretty Target where pp = undefined

instance Pretty Term where pp = undefined

instance Pretty TimeoutClause where pp = undefined

instance Pretty TypeConversion where pp = undefined

instance Pretty TypeDeclaration where pp = undefined

instance Pretty TypeDefinition where pp = undefined

instance Pretty TypeMark where pp = undefined

instance Pretty UnconstrainedArrayDefinition where pp = undefined

instance Pretty UseClause where pp = undefined

instance Pretty VariableAssignmentStatement where pp = undefined

instance Pretty VariableDeclaration where pp = undefined

instance Pretty WaitStatement where pp = undefined

instance Pretty Waveform where pp = undefined

instance Pretty WaveformElement where pp = undefined

{-
abstract_literal ::= decimal_literal | based_literal

access_type_definition ::= ACCESS subtype_indication

actual_designator ::=
	expression
	| signal_name
	| variable_name
	| file_name
	| OPEN

actual_parameter_part ::= parameter_association_list

actual_part ::=
	actual_designator
	| function_name ( actual_designator )
	| type_mark ( actual_designator )

adding_operator ::= + | -  | &

aggregate ::=
	( element_association { , element_association } )

alias_declaration ::=
	ALIAS alias_designator [ : subtype_indication ] IS name [ signature ] 	;

alias_designator ::= identifier | character_literal | operator_symbol

allocator ::=
	NEW subtype_indication
	| NEW qualified_expression

architecture_body ::=
	ARCHITECTURE identifier OF entity_name IS
		architecture_declarative_part
	BEGIN
		architecture_statement_part
	END [ ARCHITECTURE ] [ architecture_simple_name ] ;

architecture_declarative_part ::=
	{ block_declarative_item }

architecture_statement_part ::=
	{ concurrent_statement }

array_type_definition ::=
	unconstrained_array_definition	|   constrained_array_definition

assertion ::=
	ASSERT condition
		[ REPORT expression ]
		[ SEVERITY expression ]

assertion_statement ::=	 [ label : ] assertion ;

association_element ::=
	[ formal_part => ] actual_part

association_list ::=
	association_element { , association_element }

attribute_declaration ::=
	ATTRIBUTE identifier : type_mark ;

attribute_designator ::= attribute_simple_name

attribute_name ::=
	prefix [ signature ] ' attribute_designator [ ( expression ) ]

attribute_specification ::=
	ATTRIBUTE attribute_designator OF entity_specification IS expression ;

base ::= integer

base_specifier ::=  B | O | X

base_unit_declaration ::= identifier ;

based_integer ::=
	extended_digit { [ underline ] extended_digit }

based_literal ::=
	base # based_integer [ . based_integer ] # [ exponent ]

basic_character ::=
	basic_graphic_character | format_effector

basic_graphic_character ::=
	upper_case_letter | digit | special_character| space_character

basic_identifier ::=
	letter { [ underline ] letter_or_digit }

binding_indication ::=
	[ USE entity_aspect ]
	[ generic_map_aspect ]
	[ port_map_aspect ]

bit_string_literal ::=	base_specifier " bit_value "

bit_value ::= extended_digit { [ underline ] extended_digit }

block_configuration ::=
	FOR block_specification
		{ use_clause }
		{ configuration_item }
	END FOR ;

block_declarative_item ::=
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| shared_variable_declaration
	| file_declaration
	| alias_declaration
	| component_declaration
	| attribute_declaration
	| attribute_specification
	| configuration_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration

block_declarative_part ::=
	{ block_declarative_item }

block_header ::=
	[ generic_clause
	[ generic_map_aspect ; ] ]
	[ port_clause
	[ port_map_aspect ; ] ]

block_specification ::=
	architecture_name
	| block_statement_label
	| generate_statement_label [ ( index_specification ) ]

block_statement ::=
	block_label :
		BLOCK [ ( guard_expression ) ] [ IS ]
			block_header
			block_declarative_part
		BEGIN
			block_statement_part
		END BLOCK [ block_label ] ;

block_statement_part ::=
	{ concurrent_statement }

case_statement ::=
	[ case_label : ]
		CASE expression IS
			case_statement_alternative
			{ case_statement_alternative }
		END CASE [ case_label ] ;

case_statement_alternative ::=
	WHEN choices =>
		sequence_of_statements

character_literal ::= ' graphic_character '

choice ::=
	simple_expression
	| discrete_range
	| element_simple_name
	| OTHERS

choices ::= choice { | choice }

component_configuration ::=
	FOR component_specification
		[ binding_indication ; ]
		[ block_configuration ]
	END FOR ;

component_declaration ::=
	COMPONENT identifier [ IS ]
		[ local_generic_clause ]
		[ local_port_clause ]
	END COMPONENT [ component_simple_name ] ;

component_instantiation_statement ::=
	instantiation_label :
		instantiated_unit
			[ generic_map_aspect ]
			[ port_map_aspect ] ;

component_specification ::=
	instantiation_list : component_name

composite_type_definition ::=
	array_type_definition
	| record_type_definition

concurrent_assertion_statement ::=
	[ label : ] [ POSTPONED ] assertion ;

concurrent_procedure_call_statement ::=
	[ label : ] [ POSTPONED ] procedure_call ;

concurrent_signal_assignment_statement ::=
	  [ label : ] [ POSTPONED ] conditional_signal_assignment
	| [ label : ] [ POSTPONED ] selected_signal_assignment

concurrent_statement ::=
	block_statement
	| process_statement
	| concurrent_procedure_call_statement
	| concurrent_assertion_statement
	| concurrent_signal_assignment_statement
	| component_instantiation_statement
	| generate_statement

condition ::= boolean_expression

condition_clause ::= UNTIL condition

conditional_signal_assignment ::=
	target	<= options conditional_waveforms ;

conditional_waveforms ::=
	{ waveform WHEN condition ELSE }
	waveform [ WHEN condition ]

configuration_declaration ::=
	CONFIGURATION identifier OF entity_name IS
		configuration_declarative_part
		block_configuration
	END [ CONFIGURATION ] [ configuration_simple_name ] ;

configuration_declarative_item ::=
	use_clause
	| attribute_specification
	| group_declaration

configuration_declarative_part ::=
	{ configuration_declarative_item }

configuration_item ::=
	block_configuration
	| component_configuration

configuration_specification ::=
	FOR component_specification binding_indication ;

constant_declaration ::=
	CONSTANT identifier_list : subtype_indication [ := expression ] ;

constrained_array_definition ::=
	ARRAY index_constraint OF element_subtype_indication

constraint ::=
	range_constraint
	| index_constraint

context_clause ::= { context_item }

context_item ::=
	library_clause
	| use_clause

decimal_literal ::= integer [ . integer ] [ exponent ]

declaration ::=
	type_declaration
	| subtype_declaration
	| object_declaration
	| interface_declaration
	| alias_declaration
	| attribute_declaration
	| component_declaration
	| group_template_declaration
	| group_declaration
	| entity_declaration
	| configuration_declaration
	| subprogram_declaration
	| package_declaration

delay_mechanism ::=
	TRANSPORT
	| [ REJECT time_expression ] INERTIAL

design_file ::= design_unit { design_unit }

design_unit ::= context_clause library_unit

designator ::= identifier  |  operator_symbol

direction ::= TO | DOWNTO

disconnection_specification ::=
	DISCONNECT guarded_signal_specification AFTER time_expression ;

discrete_range ::= discrete_subtype_indication | range

element_association ::=
	[ choices => ] expression

element_declaration ::=
	identifier_list : element_subtype_definition ;

element_subtype_definition ::= subtype_indication

entity_aspect ::=
	  ENTITY entity_name [ ( architecture_identifier) ]
	| CONFIGURATION configuration_name
	| OPEN

entity_class ::=
	ENTITY	     | ARCHITECTURE  | CONFIGURATION
	| PROCEDURE  | FUNCTION	     | PACKAGE
	| TYPE	     | SUBTYPE	     | CONSTANT
	| SIGNAL     | VARIABLE	     | COMPONENT
	| LABEL	     | LITERAL	     | UNITS
	| GROUP	     | FILE

entity_class_entry ::=	entity_class [ <> ]

entity_class_entry_list ::=
	entity_class_entry { , entity_class_entry }

entity_declaration ::=
	ENTITY identifier IS
		entity_header
		entity_declarative_part
      [ BEGIN
		entity_statement_part ]
	END [ ENTITY ] [ entity_simple_name ] ;

entity_declarative_item ::=
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| shared_variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration

entity_declarative_part ::=
	{ entity_declarative_item }

entity_designator ::= entity_tag [ signature ]

entity_header ::=
	[ formal_generic_clause ]
	[ formal_port_clause ]

entity_name_list ::=
	entity_designator { , entity_designator }
	| OTHERS
	| ALL

entity_specification ::=
	entity_name_list : entity_class

entity_statement ::=
	concurrent_assertion_statement
	| passive_concurrent_procedure_call_statement
	| passive_process_statement

entity_statement_part ::=
	{ entity_statement }

entity_tag ::=	simple_name | character_literal | operator_symbol

enumeration_literal ::= identifier | character_literal

enumeration_type_definition ::=
	( enumeration_literal { , enumeration_literal } )

exit_statement ::=
	[ label : ] EXIT [ loop_label ] [ WHEN condition ] ;


exponent ::= E [ + ] integer | E - integer

expression ::=
	  relation { AND relation }
	| relation { OR relation }
	| relation { XOR relation }
	| relation [ NAND relation ]
	| relation [ NOR relation ]
	| relation { XNOR relation }

extended_digit ::= digit | letter

extended_identifier ::=
	\ graphic_character { graphic_character } \

factor ::=
	primary [ ** primary ]
	| ABS primary
	| NOT primary

file_declaration ::=
	FILE identifier_list : subtype_indication file_open_information ] ;

file_logical_name ::= string_expression

file_open_information ::=
	[ OPEN file_open_kind_expression ] IS file_logical_name

file_type_definition ::=
	FILE  OF type_mark

floating_type_definition ::=  range_constraint

formal_designator ::=
	generic_name
	| port_name
	| parameter_name

formal_parameter_list ::= parameter_interface_list

formal_part ::=
	formal_designator
	| function_name ( formal_designator )
	| type_mark ( formal_designator )

full_type_declaration ::=
	TYPE identifier IS type_definition ;

function_call ::=
	function_name [ ( actual_parameter_part ) ]

generate_statement ::=
	generate_label :
		generation_scheme GENERATE
			[ { block_declarative_item }
		BEGIN ]
			{ concurrent_statement }
		END GENERATE [ generate_label ] ;

generation_scheme ::=
	FOR generate_parameter_specification
	| IF condition

generic_clause ::=
	GENERIC ( generic_list ) ;

generic_list ::= generic_interface_list

generic_map_aspect ::=
	GENERIC MAP ( generic_association_list )

graphic_character ::=
	basic_graphic_character	 | lower_case_letter | other_special_character

group_constituent ::= name | character_literal

group_constituent_list ::= group_constituent { , group_constituent }

group_template_declaration ::=
	GROUP identifier IS ( entity_class_entry_list ) ;

group_declaration ::=
	GROUP identifier : group_template_name ( group_constituent_list ) ;

guarded_signal_specification ::=
	guarded_signal_list : type_mark

identifier ::=
	basic_identifier | extended_identifier

identifier_list ::= identifier { , identifier }

if_statement ::=
	[ if_label : ]
		IF condition THEN
			sequence_of_statements
		{ ELSIF condition THEN
			sequence_of_statements }
		[ ELSE
			sequence_of_statements ]
		END IF [ if_label ] ;

incomplete_type_declaration ::=	 TYPE identifier ;

index_constraint ::= ( discrete_range { , discrete_range } )

index_specification ::=
	discrete_range
	| static_expression

index_subtype_definition ::= type_mark range <>

indexed_name ::= prefix ( expression { , expression } )

instantiated_unit ::=
	[ COMPONENT ] component_name
	| ENTITY entity_name [ ( architecture_identifier ) ]
	| CONFIGURATION configuration_name

instantiation_list ::=
	instantiation_label { , instantiation_label }
	| OTHERS
	| ALL

integer ::= digit { [ underline ] digit }

integer_type_definition ::= range_constraint

interface_constant_declaration ::=
	[ CONSTANT ] identifier_list : [ IN ] subtype_indication [ := static_expression ]

interface_declaration ::=
	interface_constant_declaration
	| interface_signal_declaration
	| interface_variable_declaration
	| interface_file_declaration

interface_element ::= interface_declaration

interface_file_declaration ::=
	FILE identifier_list : subtype_indication

interface_list ::=
	interface_element { ; interface_element }

interface_signal_declaration ::=
	[SIGNAL] identifier_list : [ mode ] subtype_indication [ BUS ] [ := static_expression ]

interface_variable_declaration ::=
	[VARIABLE] identifier_list : [ mode ] subtype_indication [ := static_expression ]

iteration_scheme ::=
	WHILE condition
	| FOR loop_parameter_specification

label ::= identifier

letter ::= upper_case_letter | lower_case_letter

letter_or_digit ::= letter | digit

library_clause ::= LIBRARY logical_name_list ;

library_unit ::=
	primary_unit
	| secondary_unit

literal ::=
	numeric_literal
	| enumeration_literal
	| string_literal
	| bit_string_literal
	| NULL

logical_name ::= identifier

logical_name_list ::= logical_name { , logical_name }

logical_operator ::= AND | OR | NAND | NOR | XOR | XNOR

loop_statement ::=
	[ loop_label : ]
		[ iteration_scheme ] LOOP
			sequence_of_statements
		END LOOP [ loop_label ] ;

miscellaneous_operator ::= ** | ABS | NOT

mode ::= IN | OUT | INOUT | BUFFER | LINKAGE

multiplying_operator ::= * | / | MOD | REM

name ::=
	simple_name
	| operator_symbol
	| selected_name
	| indexed_name
	| slice_name
	| attribute_name

next_statement ::=
	[ label : ] NEXT [ loop_label ] [ WHEN condition ] ;

null_statement ::= [ label : ] NULL ;

numeric_literal ::=
	abstract_literal
	| physical_literal

object_declaration ::=
	constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration

operator_symbol ::= string_literal

options ::= [ GUARDED ] [ delay_mechanism ]

package_body ::=
	PACKAGE body package_simple_name IS
		package_body_declarative_part
	END [ PACKAGE BODY ] [ package_simple_name ] ;

package_body_declarative_item ::=
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| shared_variable_declaration
	| file_declaration
	| alias_declaration
	| use_clause
	| group_template_declaration
	| group_declaration

package_body_declarative_part ::=
	{ package_body_declarative_item }

package_declaration ::=
	PACKAGE identifier IS
		package_declarative_part
	END [ PACKAGE ] [ package_simple_name ] ;

package_declarative_item ::=
	subprogram_declaration
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| shared_variable_declaration
	| file_declaration
	| alias_declaration
	| component_declaration
	| attribute_declaration
	| attribute_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration

package_declarative_part ::=
	{ package_declarative_item }

parameter_specification ::=
	identifier IN discrete_range

physical_literal ::= [ abstract_literal ] unit_name

physical_type_definition ::=
	range_constraint
		UNITS
			base_unit_declaration
			{ secondary_unit_declaration }
		END UNITS [ physical_type_simple_name ]

port_clause ::=
	PORT ( port_list ) ;

port_list ::= port_interface_list

port_map_aspect ::=
	PORT MAP ( port_association_list )

prefix ::=
	name
	| function_call

primary ::=
	name
	| literal
	| aggregate
	| function_call
	| qualified_expression
	| type_conversion
	| allocator
	| ( expression )

primary_unit ::=
	entity_declaration
	| configuration_declaration
	| package_declaration

procedure_call ::= procedure_name [ ( actual_parameter_part ) ]

procedure_call_statement ::=
	[ label : ] procedure_call ;

process_declarative_item ::=
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| use_clause
	| group_template_declaration
	| group_declaration

process_declarative_part ::=
	{ process_declarative_item }

process_statement ::=
	[ process_label : ]
		[ POSTPONED ] PROCESS [ ( sensitivity_list ) ] [ IS ]
			process_declarative_part
		BEGIN
			process_statement_part
		END [ POSTPONED ] PROCESS [ process_label ] ;

process_statement_part ::=
	{ sequential_statement }

qualified_expression ::=
	type_mark ' ( expression )
	| type_mark ' aggregate

range ::=
	range_attribute_name
	| simple_expression direction simple_expression

range_constraint ::= range range

record_type_definition ::=
	RECORD
		element_declaration
		{ element_declaration }
	END RECORD [ record_type_simple_name ]

relation ::=
	shift_expression [ relational_operator shift_expression ]

relational_operator ::=	  =  |	/=  |  <  |  <=	 |  >  |  >=

report_statement ::=
	[ label : ]
		REPORT expression
			[ SEVERITY expression ] ;

return_statement ::=
	[ label : ] RETURN [ expression ] ;

scalar_type_definition ::=
	enumeration_type_definition   | integer_type_definition
	| floating_type_definition	  | physical_type_definition

secondary_unit ::=
	architecture_body
	| package_body

secondary_unit_declaration ::=	identifier = physical_literal ;

selected_name ::= prefix . suffix

selected_signal_assignment ::=
	WITH expression SELECT
		target	<= options selected_waveforms ;

selected_waveforms ::=
	{ waveform WHEN choices , }
	waveform WHEN choices

sensitivity_clause ::=	ON sensitivity_list

sensitivity_list ::= signal_name { , signal_name }

sequence_of_statements ::=
	{ sequential_statement }

sequential_statement ::=
	wait_statement
	| assertion_statement
	| report_statement
	| signal_assignment_statement
	| variable_assignment_statement
	| procedure_call_statement
	| if_statement
	| case_statement
	| loop_statement
	| next_statement
	| exit_statement
	| return_statement
	| null_statement

shift_expression ::=
	simple_expression [ shift_operator simple_expression ]

shift_operator ::= SLL | SRL | SLA | SRA | ROL | ROR

sign ::= + | -

signal_assignment_statement ::=
	[ label : ] target <= [ delay_mechanism ] waveform ;

signal_declaration ::=
	signal identifier_list : subtype_indication [ signal_kind ] [ := expression ] ;

signal_kind ::=	 REGISTER  |  BUS

signal_list ::=
	signal_name { , signal_name }
	| OTHERS
	| ALL

signature ::= [ [ type_mark { , type_mark } ] [ return type_mark ] ]

simple_expression ::=
	[ sign ] term { adding_operator term }

simple_name ::=	 identifier

slice_name ::=	prefix ( discrete_range )

string_literal ::= " { graphic_character } "

subprogram_body ::=
	subprogram_specification IS
		subprogram_declarative_part
	BEGIN
		subprogram_statement_part
	END [ subprogram_kind ] [ designator ] ;

subprogram_declaration ::=
	subprogram_specification ;

subprogram_declarative_item ::=
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| use_clause
	| group_template_declaration
	| group_declaration

subprogram_declarative_part ::=
	{ subprogram_declarative_item }

subprogram_kind ::= PROCEDURE | FUNCTION

subprogram_specification ::=
	PROCEDURE designator [ ( formal_parameter_list ) ]
	| [ PURE | IMPURE ]  FUNCTION designator [ ( formal_parameter_list ) ]
		RETURN type_mark

subprogram_statement_part ::=
	{ sequential_statement }

subtype_declaration ::=
	SUBTYPE identifier IS subtype_indication ;

subtype_indication ::=
	[ resolution_function_name ] type_mark [ constraint ]

suffix ::=
	simple_name
	| character_literal
	| operator_symbol
	| ALL

target ::=
	name
	| aggregate

term ::=
	factor { multiplying_operator factor }

timeout_clause ::= FOR time_expression

type_conversion ::= type_mark ( expression )

type_declaration ::=
	full_type_declaration
	| incomplete_type_declaration

type_definition ::=
	scalar_type_definition
	| composite_type_definition
	| access_type_definition
	| file_type_definition

type_mark ::=
	type_name
	| subtype_name

unconstrained_array_definition ::=
	ARRAY ( index_subtype_definition { , index_subtype_definition } )
		OF element_subtype_indication

use_clause ::=
	USE selected_name { , selected_name } ;

variable_assignment_statement ::=
	[ label : ] target  := expression ;

variable_declaration ::=
	[ SHARED ] VARIABLE identifier_list : subtype_indication [ := expression ] ;

wait_statement ::=
	[ label : ] WAIT [ sensitivity_clause ] [ condition_clause ] [ timeout_clause ] ;

waveform ::=
	waveform_element { , waveform_element }
	| UNAFFECTED

waveform_element ::=
	value_expression [ AFTER time_expression ]
	| NULL [ AFTER time_expression ]
-}

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

commaSep :: [Doc] -> Doc
commaSep = hsep . punctuate comma

pipeSep  :: [Doc] -> Doc
pipeSep  = hsep . punctuate (char '|')

semiSep  :: [Doc] -> Doc
semiSep  = hsep . punctuate semi

--------------------------------------------------------------------------------

when :: Bool -> Doc -> Doc
when b a = if b then a else empty

cond :: Pretty a => (Doc -> Doc) -> Maybe a -> Doc
cond f = maybe empty (f . pp)

condR :: Pretty a => Doc -> Maybe a -> Doc
condR s m = cond (flip (<+>) s) m

condL :: Pretty a => Doc -> Maybe a -> Doc
condL s m = cond ((<+>) s) m

label :: Pretty a => Maybe a -> Doc
label = cond (flip (<+>) colon)

indent :: Doc -> Doc
indent = nest 4

hangs  :: Doc -> Doc -> Doc
hangs  = flip hang 4

--------------------------------------------------------------------------------

postponed :: Pretty a =>  Maybe Label -> Bool -> a -> Doc
postponed l b a = cond (flip (<+>) colon) l <+> when b (text "POSTPONED") <+> pp a
