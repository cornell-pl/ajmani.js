module Database.SQL where

import Database.HsSqlPpp.Ast

class InterpretDB a where
  interpretDB :: a -> Database -> Database

class InterpretT a where
  interpretT  :: a -> Table -> Table

instance InterpretDB Statement where
  -- interpretDB ( AlterSequence Annotation Name Name ) db =
  -- interpretDB ( AlterTable Annotation Name AlterTableActionList ) db =
  -- interpretDB ( AntiStatement String ) db =
  -- interpretDB ( Assignment Annotation Name ScalarExpr ) db =
  -- interpretDB ( Block Annotation (Maybe String) VarDefList StatementList ) db =
  -- interpretDB ( CaseStatement Annotation ScalarExprListStatementListPairList StatementList ) db =
  -- interpretDB ( CaseStatementSimple Annotation ScalarExpr ScalarExprListStatementListPairList StatementList ) db =
  -- interpretDB ( ContinueStatement Annotation (Maybe String) ) db =
  -- interpretDB ( Copy Annotation Name [NameComponent] CopySource ) db =
  -- interpretDB ( CopyData Annotation String ) db =
  -- interpretDB ( CreateDomain Annotation Name TypeName String MaybeBoolExpr ) db =
  -- interpretDB ( CreateFunction Annotation Name ParamDefList TypeName Replace Language FnBody Volatility ) db =
  -- interpretDB ( CreateLanguage Annotation String ) db =
  -- interpretDB ( CreateSequence Annotation Name Integer Integer Integer Integer Integer ) db =
  -- interpretDB ( CreateTable Annotation Name AttributeDefList ConstraintList ) db =
  -- interpretDB ( CreateTableAs Annotation Name QueryExpr ) db =
  -- interpretDB ( CreateTrigger Annotation NameComponent TriggerWhen [TriggerEvent] Name TriggerFire Name ScalarExprList ) db =
  -- interpretDB ( CreateType Annotation Name TypeAttributeDefList ) db =
  -- interpretDB ( CreateView Annotation Name MaybeNameComponentList QueryExpr ) db =
  -- interpretDB ( Delete Annotation Name TableRefList MaybeBoolExpr MaybeSelectList ) db =
  -- interpretDB ( DropFunction Annotation IfExists NameTypeNameListPairList Cascade ) db =
  -- interpretDB ( DropSomething Annotation DropType IfExists [Name] Cascade ) db =
  -- interpretDB ( Execute Annotation ScalarExpr ) db =
  -- interpretDB ( ExitStatement Annotation (Maybe String) ) db =
  -- interpretDB ( ForIntegerStatement Annotation (Maybe String) NameComponent ScalarExpr ScalarExpr StatementList ) db =
  -- interpretDB ( ForQueryStatement Annotation (Maybe String) NameComponent QueryExpr StatementList ) db =
  -- interpretDB ( If Annotation ScalarExprStatementListPairList StatementList ) db =
  -- interpretDB ( Insert Annotation Name [NameComponent] QueryExpr MaybeSelectList ) db =
  -- interpretDB ( Into Annotation Bool [Name] Statement ) db =
  -- interpretDB ( LoopStatement Annotation (Maybe String) StatementList ) db =
  -- interpretDB ( Notify Annotation String ) db =
  -- interpretDB ( NullStatement Annotation ) db =
  -- interpretDB ( Perform Annotation ScalarExpr ) db =
  interpretDB ( QueryStatement ann qexpr ) db = interpretDB qexpr db
  -- interpretDB ( Raise Annotation RaiseType String ScalarExprList ) db =
  -- interpretDB ( Return Annotation MaybeScalarExpr ) db =
  -- interpretDB ( ReturnNext Annotation ScalarExpr ) db =
  -- interpretDB ( ReturnQuery Annotation QueryExpr ) db =
  -- interpretDB ( Set Annotation String [SetValue] ) db =
  -- interpretDB ( Truncate Annotation [Name] RestartIdentity Cascade ) db =
  -- interpretDB ( Update Annotation Name SetClauseList TableRefList MaybeBoolExpr MaybeSelectList ) db =
  -- interpretDB ( WhileStatement Annotation (Maybe String) ScalarExpr StatementList ) db =

instance InterpretDB QueryExpr where
  interpretDB ( CombineQueryExpr ann combineType queryExpr queryExpr ) = undefined
  interpretDB ( Select ann distinct selectList tableRefList maybeBoolExpr scalarExprList maybeBoolExpr scalarExprDirectionPairList maybeScalarExpr maybeScalarExpr ) = undefined
  interpretDB ( Values ann scalarExprListList ) = undefined
  interpretDB ( WithQueryExpr ann withQueryList queryExpr ) = undefined
  