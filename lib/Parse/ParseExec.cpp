//===-- ParseExec.cpp - Parse Executable Construct ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Functions to parse the executable construct (R213).
//
//===----------------------------------------------------------------------===//

#include "flang/Parse/Parser.h"
#include "flang/AST/Decl.h"
#include "flang/AST/Expr.h"
#include "flang/AST/Stmt.h"
#include "flang/Basic/TokenKinds.h"
#include "flang/Sema/DeclSpec.h"
#include "flang/Sema/Sema.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
using namespace flang;

/// ParseExecutableConstruct - Parse the executable construct.
///
///   [R213]:
///     executable-construct :=
///         action-stmt
///      or associate-construct
///      or block-construct
///      or case-construct
///      or critical-construct
///      or do-construct
///      or forall-construct
///      or if-construct
///      or select-type-construct
///      or where-construct
StmtResult Parser::ParseExecutableConstruct() {
  StmtResult SR = ParseActionStmt();
  if (!SR.isUsable()) return StmtResult();

  return SR;
}

/// ParseActionStmt - Parse an action statement.
///
///   [R214]:
///     action-stmt :=
///         allocate-stmt
///      or assignment-stmt
///      or backspace-stmt
///      or call-stmt
///      or close-stmt
///      or continue-stmt
///      or cycle-stmt
///      or deallocate-stmt
///      or end-function-stmt
///      or end-mp-subprogram-stmt
///      or end-program-stmt
///      or end-subroutine-stmt
///      or endfile-stmt
///      or error-stop-stmt
///      or exit-stmt
///      or flush-stmt
///      or forall-stmt
///      or goto-stmt
///      or if-stmt
///      or inquire-stmt
///      or lock-stmt
///      or nullify-stmt
///      or open-stmt
///      or pointer-assignment-stmt
///      or print-stmt
///      or read-stmt
///      or return-stmt
///      or rewind-stmt
///      or stop-stmt
///      or sync-all-stmt
///      or sync-images-stmt
///      or sync-memory-stmt
///      or unlock-stmt
///      or wait-stmt
///      or where-stmt
///      or write-stmt
///[obs] or arithmetic-if-stmt
///[obs] or computed-goto-stmt
Parser::StmtResult Parser::ParseActionStmt() {
  ParseStatementLabel();

  // This is an assignment.
  if (Tok.getIdentifierInfo() && !NextTok.isAtStartOfStatement() &&
      NextTok.is(tok::equal))
    return ParseAssignmentStmt();
      
  StmtResult SR;
  switch (Tok.getKind()) {
  default: assert(false && "Unknown statement type!"); break;
  case tok::kw_PRINT:
    return ParsePrintStmt();

  case tok::kw_END:
    // TODO: All of the end-* stmts.
    break;
  case tok::kw_ENDFUNCTION:
  case tok::kw_ENDPROGRAM:
  case tok::kw_ENDSUBPROGRAM:
  case tok::kw_ENDSUBROUTINE:
    // Handle in parent.
    return StmtResult();
  }

  return SR;
}

/// ParseAssignmentStmt
///   [R732]:
///     assignment-stmt :=
///         variable = expr
Parser::StmtResult Parser::ParseAssignmentStmt() {
  ExprResult LHS = ParseExpression();

  assert(Tok.is(tok::equal) && "Not a valid assignment statement!");
  EatIfPresent(tok::equal);

  ExprResult RHS = ParseExpression();
  return Actions.ActOnAssignmentStmt(Context, LHS, RHS, StmtLabelTok);
}

/// ParsePrintStatement
///   [R912]:
///     print-stmt :=
///         PRINT format [, output-item-list]
///   [R915]:
///     format :=
///         default-char-expr
///      or label
///      or *
Parser::StmtResult Parser::ParsePrintStmt() {
  SMLoc Loc = Tok.getLocation();
  Lex();

  SMLoc FormatLoc = Tok.getLocation();
  FormatSpec *FS = 0;
  if (EatIfPresent(tok::star)) {
    FS = Actions.ActOnStarFormatSpec(Context, FormatLoc);
  }

  // TODO: Parse the FORMAT default-char-expr & label.

  if (!EatIfPresent(tok::comma)) {
    Diag.ReportError(Tok.getLocation(),
                     "expected ',' after format specifier in PRINT statement");
    return StmtResult(true);
  }

  SmallVector<ExprResult, 4> OutputItemList;
  while (!Tok.isAtStartOfStatement()) {
    OutputItemList.push_back(ParseExpression());
    if (!EatIfPresent(tok::comma))
      break;
  }

  return Actions.ActOnPrintStmt(Context, Loc, FS, OutputItemList, StmtLabelTok);
}

/// ParseEND_PROGRAMStmt - Parse the END PROGRAM statement.
///
///   [R1103]:
///     end-program-stmt :=
///         END [ PROGRAM [ program-name ] ]
Parser::StmtResult Parser::ParseEND_PROGRAMStmt() {
  llvm::SMLoc Loc = Tok.getLocation();
  if (Tok.isNot(tok::kw_END) && Tok.isNot(tok::kw_ENDPROGRAM)) {
    Diag.ReportError(Tok.getLocation(),
                     "expected 'END PROGRAM' statement");
    return StmtResult();
  }
  Lex();

  const IdentifierInfo *IDInfo = 0;
  llvm::SMLoc NameLoc;
  if (Tok.is(tok::identifier) && !Tok.isAtStartOfStatement()) {
    IDInfo = Tok.getIdentifierInfo();
    NameLoc = Tok.getLocation();
    Lex(); // Eat the ending token.
  }

  return Actions.ActOnENDPROGRAM(Context, IDInfo, Loc, NameLoc, StmtLabelTok);
}
