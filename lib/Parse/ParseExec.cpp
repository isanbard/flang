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
#include "flang/Basic/Actions.h"
#include "flang/Basic/DeclSpec.h"
#include "flang/Basic/TokenKinds.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
using namespace flang;

/// ParseExecutableConstruct - Parse the executable construct.
///
///   R213:
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
bool Parser::ParseExecutableConstruct() {
  if (ParseActionStmt())
    return true;
  return false;
}

/// ParseActionStmt - Parse an action statement.
///
///   R214:
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
bool Parser::ParseActionStmt() {
  ParseStatementLabel();

  StmtResult SR;
  switch (Tok.getKind()) {
  default:
    // Assignment Statement.
    //   R732:
    //     assignment-stmt :=
    //         variable = expr
    break;
  case tok::kw_ALLOCATE:
    Lex();
    break;
  case tok::kw_END:
    // TODO: All of the end-* stmts.
    break;
  case tok::kw_ENDPROGRAM:
    SR = ParseEND_PROGRAMStmt();
    break;
  }

  return false;
}

/// ParseEND_PROGRAMStmt - Parse the END PROGRAM statement.
///
///   [11.1] R1103:
///     end-program-stmt :=
///         END [ PROGRAM [ program-name ] ]
Parser::StmtResult Parser::ParseEND_PROGRAMStmt() {
  bool sawEnd = Tok.is(tok::kw_END);
  bool sawEndProgram = Tok.is(tok::kw_ENDPROGRAM);

  if (!sawEnd && !sawEndProgram) {
    Diag.ReportError(Tok.getLocation(),
                     "expected 'END PROGRAM' statement");
    return StmtResult();
  }
  Lex();

  llvm::SMLoc TokLoc = Tok.getLocation();
  if (Tok.is(tok::eof))
    // The program name wasn't specified in the 'END PROGRAM' statement.
    return Actions.ActOnEND_PROGRAM(TokLoc, 0, StmtLabelTok);

  const IdentifierInfo *IDInfo = Tok.getIdentifierInfo();
  if (!Tok.is(tok::eof)) Lex(); // Eat the ending token.
  return Actions.ActOnEND_PROGRAM(TokLoc, IDInfo, StmtLabelTok);
}
