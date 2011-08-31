//===-- Actions.cpp - Actions Invoked by Parser ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Actions invoked by the parser.
//
//===----------------------------------------------------------------------===//

#include "flang/Basic/Actions.h"
#include "flang/Basic/DeclSpec.h"
#include "flang/Basic/Diagnostic.h"
#include "flang/Basic/Token.h"
#include "flang/AST/ASTContext.h"
#include "flang/AST/Decl.h"
#include "flang/AST/Expr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Twine.h"
using namespace fortran;

static inline void PrintStmtLabel(Token &StmtLabel) {
  if (StmtLabel.isNot(tok::statement_label)) return;
  llvm::StringRef SLabel(StmtLabel.getLiteralData(), StmtLabel.getLength());
  llvm::outs() << " stmt_label=\"" << SLabel << "\"";
}

//===----------------------------------------------------------------------===//
// Anchor the Action class
//===----------------------------------------------------------------------===//

Action::~Action() {
}

StmtResult PrintAction::ActOnPROGRAM(const IdentifierInfo *ProgName,
                                     Token &StmtLabel) {
  ProgramName = ProgName;
  llvm::outs() << "<program";
  PrintStmtLabel(StmtLabel);
  if (ProgName)
    llvm::outs() << " name=\"" << ProgName->getName() << "\"";
  llvm::outs() << ">\n";
  ++Indent;
  return StmtResult();          // FIXME:
}

StmtResult PrintAction::ActOnEND_PROGRAM(llvm::SMLoc Loc,
                                         const IdentifierInfo *ProgName,
                                         Token &StmtLabel) {
  Indent = 0;

  // This name, if specified, must match the name specified on the 'PROGRAM'
  // statement.
  llvm::StringRef Name = (ProgName ? ProgName->getName() : "");
  if (Name.size() != 0 && ProgName != ProgramName) {
    // FIXME: Use a real error reporting mechanism here.
    Diag.ReportError(Loc, llvm::Twine("expected label '") +
                     ProgramName->getName() +
                     "' for END PROGRAM statement");
    return StmtResult();
  }

  llvm::outs() << "</program";
  PrintStmtLabel(StmtLabel);
  llvm::outs() << ">\n";
  return StmtResult();          // FIXME:
}

StmtResult PrintAction::ActOnIMPORT(llvm::ArrayRef<IdentifierInfo*> Names,
                                    Token &StmtLabel) {
  llvm::outs().indent(Indent * 2) << "<import";
  PrintStmtLabel(StmtLabel);
  llvm::outs() << ">\n";

  ++Indent;
  for (llvm::ArrayRef<IdentifierInfo*>::iterator
         I = Names.begin(), E = Names.end(); I != E; ++I)
    llvm::outs().indent(Indent * 2) << "<item>" << (*I)->getName()
                                    << "</item>\n";
  --Indent;

  llvm::outs().indent(Indent * 2) << "</import>\n";
  return StmtResult();          // FIXME:
}

StmtResult PrintAction::ActOnIMPLICIT(Token &StmtLabel) {
  llvm::outs().indent(Indent * 2) << "<implicit";
  PrintStmtLabel(StmtLabel);
  llvm::outs() << " type=\"None\"/>\n";
  return StmtResult();          // FIXME:
}

StmtResult PrintAction::ActOnUSE(UseStmt::ModuleNature MN, llvm::StringRef Name,
                                 bool OnlyList,
                                 llvm::ArrayRef<const VarDecl*> LocalNames,
                                 llvm::ArrayRef<const VarDecl*> UseNames,
                                 Token &StmtLabel) {
  llvm::outs().indent(Indent * 2) << "<use";
  PrintStmtLabel(StmtLabel);

  switch (MN) {
  default: break;
  case UseStmt::Intrinsic:
  case UseStmt::NonIntrinsic:
    llvm::outs() << " module_nature=\"";
    if (MN == UseStmt::NonIntrinsic) llvm::outs() << "NON ";
    llvm::outs() << "INTRINSIC\"";
    break;
  }
  llvm::outs() << ">\n";
  llvm::outs().indent(++Indent * 2) << "<module>" << Name << "</module>\n";

  if (OnlyList)
    llvm::outs().indent(Indent++ * 2) << "<only>\n";

  if (!UseNames.empty())
    llvm::outs().indent(Indent++ * 2) << "<map>\n";

  for (unsigned I = 0, E = LocalNames.size(); I != E; ++I) {
    llvm::outs().indent(Indent * 2) << "<name>" << *LocalNames[I] << "</name>";
    if (!UseNames.empty())
      llvm::outs() << "<to>" << *UseNames[I] << "</to>";
    llvm::outs() << '\n';
  }

  if (!UseNames.empty())
    llvm::outs().indent(--Indent * 2) << "</map>\n";

  if (OnlyList)
    llvm::outs().indent(--Indent * 2) << "</only>\n";

  llvm::outs().indent(--Indent * 2) << "</use>\n";
  return StmtResult();          // FIXME:
}

StmtResult PrintAction::
ActOnPARAMETER(llvm::ArrayRef<const IdentifierInfo*> NamedConsts,
               llvm::ArrayRef<ExprResult> ConstExprs,
               Token &StmtLabel) {
  llvm::outs().indent(Indent++ * 2) << "<parameter";
  PrintStmtLabel(StmtLabel);
  llvm::outs() << ">\n";

  for (unsigned I = 0, E = NamedConsts.size(); I != E; ++I) {
    llvm::outs().indent(Indent * 2)
      << "<name>" << NamedConsts[I]->getName() << "</name> ";
    llvm::outs().indent(Indent * 2) << "<param>";
    ConstExprs[I].get()->print(llvm::outs());
    llvm::outs() << "</param>\n";
  }

  llvm::outs().indent(--Indent * 2) << "</parameter>";
  return StmtResult();          // FIXME:
}

StmtResult PrintAction::
ActOnASYNCHRONOUS(llvm::ArrayRef<const IdentifierInfo*> ObjNames,
                  Token &StmtLabel) {
  llvm::outs().indent(Indent++ * 2) << "<asynchronous";
  PrintStmtLabel(StmtLabel);
  llvm::outs() << ">\n";

  for (unsigned I = 0, E = ObjNames.size(); I != E; ++I) {
    llvm::outs().indent(Indent * 2)
      << "<name>" << ObjNames[I]->getName() << "</name> ";
  }

  llvm::outs().indent(--Indent * 2) << "</asynchronous>";
  return StmtResult();          // FIXME:
}

QualType PrintAction::ActOnBuiltinType(ASTContext *Ctx,
                                       BuiltinType::TypeSpec TS,
                                       Expr *Kind) {
  return Ctx->getBuiltinType(TS, Kind);
}

QualType PrintAction::ActOnCharacterBuiltinType(ASTContext *Ctx, Expr *Len,
                                                Expr *Kind) {
  return Ctx->getCharacterBuiltinType(Len, Kind);
}

DeclSpec *PrintAction::ActOnTypeDeclSpec(ASTContext *Ctx) {
  return 0;
}

bool PrintAction::ActOnVarDecl(ASTContext *Ctx, DeclSpec *DTS,
                               const VarDecl *VD) {
  llvm::outs().indent(Indent * 2) << '<';
  DTS->print(llvm::outs());
  llvm::outs() << " name=\"" << VD->getIdentifier()->getName() << "\" />\n";
  return false;
}

bool PrintAction::ActOnArraySpec() {
  return false;
}

ExprResult PrintAction::ActOnDataReference(llvm::ArrayRef<ExprResult> Exprs) {
  return ExprResult();
}
