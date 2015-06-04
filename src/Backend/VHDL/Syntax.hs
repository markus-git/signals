module Backend.VHDL.Syntax where

--------------------------------------------------------------------------------
--
--                                   -- 1 --
--
--                      Design entities and configurations
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 1.1 Entiity Declarations
--------------------------------------------------------------------------------

data Entity = Entity {
    entity_ident              :: ()
  , entity_header             :: EntityHeader
  , entity_declarative_part   :: EntityDecl
  , entity_statement_part     :: Maybe EntityStmtPart
  , entity_simple_name        :: Maybe ()
  }

--------------------------------------------------------------------------------
-- ** 1.1.1 Entity haeder

data EntityHeader = EntityHeader {
    formal_generic_clause     :: Maybe [Generic]
  , formal_port_clause        :: Maybe [Port]
  }

--------------------------------------------------------------------------------
-- *** 1.1.1.1 Generics

data Generic = Generic () -- generic_interface_list

--------------------------------------------------------------------------------
-- *** 1.1.1.2 Ports

data Port = Port () -- port_interface_list

data PortMode =
    In
  | Out
  | InOut
  | Buffer
  | Linkage

--------------------------------------------------------------------------------
-- ** 1.1.2 Entity declarative part

data EntityDecl = EntityDecl [EntityDeclItem]

data EntityDeclItem = EntityDeclItem ()
{-
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
-}

--------------------------------------------------------------------------------
-- ** 1.1.3 Entity statement part

type EntityStmtPart = [EntityStmt]

data EntityStmt = EntityStmt ()
{-
    concurrent_assertion_statement
  | passive_concurrent_procedureCall
  | passive_process_statement
-}

--------------------------------------------------------------------------------
-- * 1.2 Arcitecture bodies
--------------------------------------------------------------------------------

data ArchitectureBody = ArchitectureBody {
    archi_ident            :: ()
  , archi_entity_name      :: ()
  , archi_declarative_part :: ArchitectureDeclPart
  , archi_statement_part   :: ArchitectureStmtPart
  , archi_simple_name      :: Maybe ()
  }

--------------------------------------------------------------------------------
-- ** 1.2.1 Architecture declarative part

type ArchitectureDeclPart = [BlockDeclItem]

data BlockDeclItem = BlockDeclItem () -- clause 4,5,10
{-
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
-}

--------------------------------------------------------------------------------
-- ** 1.2.2 Architecture statement part

type ArchitectureStmtPart = [ConcurrentStmt]

data ConcurrentStmt = ConcurrentStmt () -- clause 9

--------------------------------------------------------------------------------
-- * 1.3 Configuration declarations
--------------------------------------------------------------------------------

data ConfigurationDecl = ConfigurationDecl {
    config_ident               :: ()
  , config_entity_name         :: ()
  , config_declarative_part    :: ConfigurationDeclPart
  , config_block_configuration :: ()
  , config_simple_name         :: Maybe ()
  }

type ConfigurationDeclPart = [ConfigurationDeclItem]

data ConfigurationDeclItem = ConfigurationDeclItem ()
{-
    use_clause
  | attribute_specification
  | group_declaration
-}

--------------------------------------------------------------------------------
-- ** 1.3.1 Block configuration

data BlockConfiguration = BlockConfiguration {
    block_specification      :: BlockSpecification
  , block_use_clause         :: [()] -- use_claus
  , block_configuration_item :: [ConfigurationItem]
  }

data BlockSpecification = BlockSpecification ()
{-
    architecture_name
  | block_statement_label
  | generate_statement_label [(index_specification)]
-}

data IndexSpecification = IndexSpecification ()
{-
    discrete_range
  | static_expression
-}

data ConfigurationItem =
    CIBlock BlockConfiguration
  | CIComp  ComponentConfiguration
{-
    block_configuration
  | component_configuration
-}

--------------------------------------------------------------------------------
-- ** 1.3.2 Component configuration

data ComponentConfiguration = ComponentConfiguration {
    comp_specification       :: ()
  , comp_binding_indication  :: Maybe ()
  , comp_block_configuration :: Maybe BlockConfiguration
  }

--------------------------------------------------------------------------------
--
--                                   -- 2 --
--
--                           Subprograms and packages
-- 
--------------------------------------------------------------------------------
