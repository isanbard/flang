//===--- StmtDumper.cpp - Dump Fortran Statements -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file dumps statements.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/StmtDumper.h"
#include "flang/AST/Expr.h"
#include "flang/AST/Stmt.h"
#include "flang/AST/Type.h"
#include "flang/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"
using namespace flang;

namespace {

class StmtVisitor {
  raw_ostream &OS;
public:
  StmtVisitor(raw_ostream &os) : OS(os) {}

  void visit(Stmt*);

private:
#define VISIT(STMT) void visit(const STMT *S)
  VISIT(ProgramStmt);
  VISIT(EndProgramStmt);
  VISIT(UseStmt);
  VISIT(ImportStmt);
  VISIT(AsynchronousStmt);
  VISIT(AssignmentStmt);
  VISIT(PrintStmt);
#undef VISIT
};

} // end anonymous namespace

void StmtVisitor::visit(Stmt *S) {
#define HANDLE(STMT) \
  if (const STMT *stmt = dyn_cast<STMT>(S)) {   \
    visit(stmt);                                \
    return;                                     \
  }
  HANDLE(ProgramStmt);
  HANDLE(EndProgramStmt);
  HANDLE(UseStmt);
  HANDLE(ImportStmt);
  HANDLE(AsynchronousStmt);
  HANDLE(AssignmentStmt);
  HANDLE(PrintStmt);
#undef HANDLE
}

void StmtVisitor::visit(const ProgramStmt *S) {
  const IdentifierInfo *Name = S->getProgramName();
  OS << "(program";
  if (Name) OS << ":  '" << Name->getName() << "'";
  OS << ")\n";
}
void StmtVisitor::visit(const EndProgramStmt *S) {
  const IdentifierInfo *Name = S->getProgramName();
  OS << "(end program";
  if (Name) OS << ":  '" << Name->getName() << "'";
  OS << ")\n";
}
void StmtVisitor::visit(const UseStmt *S) {
}
void StmtVisitor::visit(const ImportStmt *S) {
}
void StmtVisitor::visit(const AsynchronousStmt *S) {
}
void StmtVisitor::visit(const AssignmentStmt *S) {
  OS << "(assignment:\n  (";
  S->getLHS()->getType().print(OS);
  OS << ")\n  (";
  S->getRHS()->getType().print(OS);
  OS << "))\n";
}
void StmtVisitor::visit(const PrintStmt *S) {
  OS << "(print:\n  [";
  OS << ")\n";
}

void flang::dump(ArrayRef<Stmt*> S) {
  StmtVisitor SV(llvm::errs());

  for (ArrayRef<Stmt*>::iterator I = S.begin(), E = S.end(); I != E; ++I)
    SV.visit(*I);
}
