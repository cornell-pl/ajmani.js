module Database.SQL where

import Database.HsSqlPpp.Ast

interpret :: Statement -> Database -> Database
-- interpret ( AlterSequence Annotation Name Name ) db =
-- interpret ( AlterTable Annotation Name AlterTableActionList ) db =
-- interpret ( AntiStatement String ) db =
-- interpret ( Assignment Annotation Name ScalarExpr ) db =
-- interpret ( Block Annotation (Maybe String) VarDefList StatementList ) db =
-- interpret ( CaseStatement Annotation ScalarExprListStatementListPairList StatementList ) db =
-- interpret ( CaseStatementSimple Annotation ScalarExpr ScalarExprListStatementListPairList StatementList ) db =
-- interpret ( ContinueStatement Annotation (Maybe String) ) db =
-- interpret ( Copy Annotation Name [NameComponent] CopySource ) db =
-- interpret ( CopyData Annotation String ) db =
-- interpret ( CreateDomain Annotation Name TypeName String MaybeBoolExpr ) db =
-- interpret ( CreateFunction Annotation Name ParamDefList TypeName Replace Language FnBody Volatility ) db =
-- interpret ( CreateLanguage Annotation String ) db =
-- interpret ( CreateSequence Annotation Name Integer Integer Integer Integer Integer ) db =
-- interpret ( CreateTable Annotation Name AttributeDefList ConstraintList ) db =
-- interpret ( CreateTableAs Annotation Name QueryExpr ) db =
-- interpret ( CreateTrigger Annotation NameComponent TriggerWhen [TriggerEvent] Name TriggerFire Name ScalarExprList ) db =
-- interpret ( CreateType Annotation Name TypeAttributeDefList ) db =
-- interpret ( CreateView Annotation Name MaybeNameComponentList QueryExpr ) db =
-- interpret ( Delete Annotation Name TableRefList MaybeBoolExpr MaybeSelectList ) db =
-- interpret ( DropFunction Annotation IfExists NameTypeNameListPairList Cascade ) db =
-- interpret ( DropSomething Annotation DropType IfExists [Name] Cascade ) db =
-- interpret ( Execute Annotation ScalarExpr ) db =
-- interpret ( ExitStatement Annotation (Maybe String) ) db =
-- interpret ( ForIntegerStatement Annotation (Maybe String) NameComponent ScalarExpr ScalarExpr StatementList ) db =
-- interpret ( ForQueryStatement Annotation (Maybe String) NameComponent QueryExpr StatementList ) db =
-- interpret ( If Annotation ScalarExprStatementListPairList StatementList ) db =
-- interpret ( Insert Annotation Name [NameComponent] QueryExpr MaybeSelectList ) db =
-- interpret ( Into Annotation Bool [Name] Statement ) db =
-- interpret ( LoopStatement Annotation (Maybe String) StatementList ) db =
-- interpret ( Notify Annotation String ) db =
-- interpret ( NullStatement Annotation ) db =
-- interpret ( Perform Annotation ScalarExpr ) db =
interpret ( QueryStatement ann qexpr ) db =
-- interpret ( Raise Annotation RaiseType String ScalarExprList ) db =
-- interpret ( Return Annotation MaybeScalarExpr ) db =
-- interpret ( ReturnNext Annotation ScalarExpr ) db =
-- interpret ( ReturnQuery Annotation QueryExpr ) db =
-- interpret ( Set Annotation String [SetValue] ) db =
-- interpret ( Truncate Annotation [Name] RestartIdentity Cascade ) db =
-- interpret ( Update Annotation Name SetClauseList TableRefList MaybeBoolExpr MaybeSelectList ) db =
-- interpret ( WhileStatement Annotation (Maybe String) ScalarExpr StatementList ) db =