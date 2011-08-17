//===--- Stmt.cpp - Fortran Statements ------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the statement objects.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/Stmt.h"
#include "flang/Basic/IdentifierTable.h"
#include "llvm/ADT/StringRef.h"
using namespace fortran;

//===----------------------------------------------------------------------===//
// Statement Base Class
//===----------------------------------------------------------------------===//

Stmt::~Stmt() {}

//===----------------------------------------------------------------------===//
// Program Statement
//===----------------------------------------------------------------------===//

ProgramStmt *ProgramStmt::Create(const IdentifierInfo *ProgName) {
  return new ProgramStmt(ProgName);
}

ProgramStmt *ProgramStmt::Create(const IdentifierInfo *ProgName,
                                 Token StmtLabelTok) {
  return new ProgramStmt(ProgName, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// EndProgram Statement
//===----------------------------------------------------------------------===//

EndProgramStmt *EndProgramStmt::Create(const IdentifierInfo *ProgName) {
  return new EndProgramStmt(ProgName);
}

EndProgramStmt *EndProgramStmt::Create(const IdentifierInfo *ProgName,
                                       Token StmtLabelTok) {
  return new EndProgramStmt(ProgName, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// Use Statement
//===----------------------------------------------------------------------===//

UseStmt::UseStmt(ModuleNature MN, const IdentifierInfo *Info,Token StmtLabelTok)
  : Stmt(Use, StmtLabelTok), ModNature(MN), ModName(Info) {}

UseStmt *UseStmt::Create(ModuleNature MN, const IdentifierInfo *Info,
                         Token StmtLabelTok) {
  return new UseStmt(MN, Info, StmtLabelTok);
}

llvm::StringRef UseStmt::getModuleName() const {
  return ModName->getName();
}

//===----------------------------------------------------------------------===//
// Import Statement
//===----------------------------------------------------------------------===//

ImportStmt::ImportStmt(Token StmtLabelTok)
  : Stmt(Import, StmtLabelTok) {
}
ImportStmt::ImportStmt(llvm::ArrayRef<const IdentifierInfo*> names,
                       Token StmtLabelTok)
  : Stmt(Import, StmtLabelTok) {
  Names.resize(names.size());
  std::copy(names.begin(), names.end(), Names.begin());
}

ImportStmt *ImportStmt::Create(Token StmtLabelTok) {
  return new ImportStmt(StmtLabelTok);
}

ImportStmt *ImportStmt::Create(llvm::ArrayRef<const IdentifierInfo*> Names,
                               Token StmtLabelTok) {
  return new ImportStmt(Names, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// Asynchronous Statement
//===----------------------------------------------------------------------===//

AsynchronousStmt::
AsynchronousStmt(llvm::ArrayRef<const IdentifierInfo*> objNames,
                 Token StmtLabelTok)
  : Stmt(Asynchronous, StmtLabelTok) {
  std::copy(objNames.begin(), objNames.end(), ObjNames.begin());
}

AsynchronousStmt *AsynchronousStmt::
Create(llvm::ArrayRef<const IdentifierInfo*> objNames, Token StmtLabelTok) {
  return new AsynchronousStmt(objNames, StmtLabelTok);
}
