//===-- Parser.h - Fortran Parser Interface ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The Fortran parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_PARSER_PARSER_H__
#define FLANG_PARSER_PARSER_H__

#include "flang/AST/ASTContext.h" // FIXME: Move to AST construction.
#include "flang/Basic/Diagnostic.h"
#include "flang/Basic/IdentifierTable.h"
#include "flang/Basic/LangOptions.h"
#include "flang/Basic/TokenKinds.h"
#include "flang/Parse/Lexer.h"
#include "flang/Sema/DeclSpec.h"
#include "flang/Sema/Ownership.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Twine.h"
#include <vector>

namespace llvm {
  class SourceMgr;
} // end namespace llvm

namespace flang {

class Action;
class ConstantExpr;
class DeclGroupRef;
class Expr;
class Parser;
class Selector;
class Sema;

/// PrettyStackTraceParserEntry - If a crash happens while the parser is active,
/// an entry is printed for it.
class PrettyStackTraceParserEntry : public llvm::PrettyStackTraceEntry {
  const Parser &FP;
public:
  PrettyStackTraceParserEntry(const Parser &fp) : FP(fp) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

/// Parser - This implements a parser for the Fortran family of languages. After
/// parsing units of the grammar, productions are invoked to handle whatever has
/// been read.
class Parser {
public:
  enum RetTy {
    Success,                //< The construct was parsed successfully
    WrongConstruct,         //< The construct we wanted to parse wasn't present
    Error                   //< There was an error parsing
  };
private:
  Lexer TheLexer;
  LangOptions Features;
  PrettyStackTraceParserEntry CrashInfo;
  llvm::SourceMgr &SrcMgr;

  /// This is the current buffer index we're lexing from as managed by the
  /// SourceMgr object.
  int CurBuffer;

  ASTContext &Context;

  /// Diag - Diagnostics for parsing errors.
  Diagnostic &Diag;

  /// Actions - These are the callbacks we invoke as we parse various constructs
  /// in the file. 
  Sema &Actions;
#if 0
  /// Actions - These are the callbacks we invoke as we parse various constructs
  /// in the file.
  Action &Actions;
#endif

  /// Tok - The current token we are parsing. All parsing methods assume that
  /// this is valid.
  Token Tok;

  /// NextTok - The next token so that we can do one level of lookahead.
  Token NextTok;

  /// StmtLabelTok - If set, this is the statement label for the statement.
  Token StmtLabelTok;

  // PrevTokLocation - The location of the token we previously consumed. This
  // token is used for diagnostics where we expected to see a token following
  // another token.
  llvm::SMLoc PrevTokLocation;

  /// Identifiers - This is mapping/lookup information for all identifiers in
  /// the program, including program keywords.
  mutable IdentifierTable Identifiers;

  /// getIdentifierInfo - Return information about the specified identifier
  /// token.
  IdentifierInfo *getIdentifierInfo(std::string &Name) const {
    return &Identifiers.get(Name);
  }

  /// ConsumeToken - Consume the current 'peek token' and lex the next one. This
  /// returns the location of the consumed token.
  llvm::SMLoc ConsumeToken() {
    PrevTokLocation = Tok.getLocation();
    TheLexer.Lex(Tok);
    return PrevTokLocation;
  }

  /// CleanLiteral - Cleans up a literal if it needs cleaning. It removes the
  /// continuation contexts and comments. Cleaning a dirty literal is SLOW!
  void CleanLiteral(Token T, std::string &NameStr);

  bool EnterIncludeFile(const std::string &Filename);

  const Token &PeekAhead() const {
    return NextTok;
  }

  void Lex();
  void ClassifyToken(Token &T);
public:
  typedef OpaquePtr<DeclGroupRef> DeclGroupPtrTy;

  typedef flang::ExprResult ExprResult;
  typedef flang::StmtResult StmtResult;

  bool isaIdentifier(const llvm::StringRef &ID) const {
    return Identifiers.isaIdentifier(ID);
  }
  bool isaKeyword(const llvm::StringRef &KW) const {
    return Identifiers.isaKeyword(KW);
  }
  bool isaBuiltin(const llvm::StringRef &BI) const {
    return Identifiers.isaBuiltin(BI);
  }

  Parser(llvm::SourceMgr &SrcMgr, const LangOptions &Opts,
         Diagnostic &D, Sema &actions);

  llvm::SourceMgr &getSourceManager() { return SrcMgr; }

  const Token &getCurToken() const { return Tok; }
  const Lexer &getLexer() const { return TheLexer; }
  Lexer &getLexer() { return TheLexer; }

  bool ParseProgramUnits();

private:
  // High-level parsing methods.
  bool ParseProgramUnit();
  bool ParseMainProgram(std::vector<StmtResult> &Body);
  bool ParseExternalSubprogram();
  bool ParseFunctionSubprogram();
  bool ParseSubroutineSubprogram();
  bool ParseModule();
  bool ParseBlockData();

  bool ParseSpecificationPart(std::vector<StmtResult> &Body);
  bool ParseImplicitPartList(std::vector<StmtResult> &Body);
  StmtResult ParseImplicitPart();
  bool ParseExecutionPart(std::vector<StmtResult> &Body);

  bool ParseDeclarationConstructList();
  bool ParseDeclarationConstruct();
  bool ParseForAllConstruct();
  StmtResult ParseExecutableConstruct();

  bool ParseTypeDeclarationStmt(SmallVectorImpl<DeclResult> &Decls);
  bool ParseProcedureDeclStmt();
  bool ParseSpecificationStmt();
  StmtResult ParseActionStmt();

  // Designator parsing methods.
  ExprResult ParseDesignator();
  ExprResult ParseArrayElement();
  ExprResult ParseArraySection();
  ExprResult ParseCoindexedNamedObject();
  ExprResult ParseComplexPartDesignator();
  ExprResult ParseStructureComponent();
  ExprResult ParseSubstring();
  ExprResult ParseDataReference();
  ExprResult ParsePartReference();

  // Stmt-level parsing methods.
  StmtResult ParsePROGRAMStmt();
  StmtResult ParseUSEStmt();
  StmtResult ParseIMPORTStmt();
  StmtResult ParseIMPLICITStmt();
  StmtResult ParsePARAMETERStmt();
  StmtResult ParseFORMATStmt();
  StmtResult ParseENTRYStmt();
  StmtResult ParseEND_PROGRAMStmt();

  // Specification statement's contents.
  StmtResult ParseACCESSStmt();
  StmtResult ParseALLOCATABLEStmt();
  StmtResult ParseASYNCHRONOUSStmt();
  StmtResult ParseBINDStmt();
  StmtResult ParseCOMMONStmt();
  StmtResult ParseDATAStmt();
  StmtResult ParseDIMENSIONStmt();
  StmtResult ParseEQUIVALENCEStmt();
  StmtResult ParseEXTERNALStmt();
  StmtResult ParseINTENTStmt();
  StmtResult ParseINTRINSICStmt();
  StmtResult ParseNAMELISTStmt();
  StmtResult ParseOPTIONALStmt();
  StmtResult ParsePOINTERStmt();
  StmtResult ParsePROTECTEDStmt();
  StmtResult ParseSAVEStmt();
  StmtResult ParseTARGETStmt();
  StmtResult ParseVALUEStmt();
  StmtResult ParseVOLATILEStmt();

  // Dynamic association.
  StmtResult ParseALLOCATEStmt();
  StmtResult ParseNULLIFYStmt();
  StmtResult ParseDEALLOCATEStmt();

  StmtResult ParseWHEREStmt();
  StmtResult ParseFORALLStmt();
  StmtResult ParseEND_FORALLStmt();

  //
  StmtResult ParseAssignmentStmt();
  StmtResult ParsePrintStmt();

  // Helper functions.
  ExprResult ParseLevel5Expr();
  ExprResult ParseEquivOperand();
  ExprResult ParseOrOperand();
  ExprResult ParseAndOperand();
  ExprResult ParseLevel4Expr();
  ExprResult ParseLevel3Expr();
  ExprResult ParseLevel2Expr();
  ExprResult ParseAddOperand();
  ExprResult ParseMultOperand();
  ExprResult ParseLevel1Expr();
  ExprResult ParsePrimaryExpr();
  ExprResult ParseExpression();

  void ParseStatementLabel();

  // Declaration construct functions
  bool ParseDeclarationTypeSpec(DeclSpec &DS);
  bool ParseTypeOrClassDeclTypeSpec(DeclSpec &DS);
  ExprResult ParseSelector(bool IsKindSel);
  bool ParseDerivedTypeSpec(DeclSpec &DS);
  bool ParseArraySpec(llvm::SmallVectorImpl<ExprResult> &Dims);
  bool ParseTypeDeclarationList(DeclSpec &DS,
                                SmallVectorImpl<DeclResult> &Decls);

  bool AssignAttrSpec(DeclSpec &DS, DeclSpec::AS Val);
  bool AssignAccessSpec(DeclSpec &DS, DeclSpec::AC Val);
  bool AssignIntentSpec(DeclSpec &DS, DeclSpec::IS Val);

  void SetKindSelector(ConstantExpr *E, StringRef Kind);

  void LexToEndOfStatement();
  bool EatIfPresent(tok::TokenKind);
};

} // end flang namespace

#endif
