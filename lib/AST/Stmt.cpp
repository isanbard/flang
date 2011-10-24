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
#include "flang/AST/ASTContext.h"
#include "flang/Basic/IdentifierTable.h"
#include "llvm/ADT/StringRef.h"
using namespace flang;

//===----------------------------------------------------------------------===//
// Statement Base Class
//===----------------------------------------------------------------------===//

Stmt::~Stmt() {}

//===----------------------------------------------------------------------===//
// Program Statement
//===----------------------------------------------------------------------===//

ProgramStmt *ProgramStmt::Create(ASTContext &C, const IdentifierInfo *ProgName,
                                 llvm::SMLoc Loc, llvm::SMLoc NameLoc,
                                 Token StmtLabelTok) {
  return new (C) ProgramStmt(ProgName, Loc, NameLoc, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// EndProgram Statement
//===----------------------------------------------------------------------===//

EndProgramStmt *EndProgramStmt::Create(ASTContext &C,
                                       const IdentifierInfo *ProgName,
                                       llvm::SMLoc Loc, llvm::SMLoc NameLoc,
                                       Token StmtLabelTok) {
  return new (C) EndProgramStmt(ProgName, Loc, NameLoc, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// Use Statement
//===----------------------------------------------------------------------===//

UseStmt::UseStmt(ModuleNature MN, const IdentifierInfo *Info,Token StmtLabelTok)
  : Stmt(Use, llvm::SMLoc(), StmtLabelTok), ModNature(MN), ModName(Info) {}

UseStmt *UseStmt::Create(ASTContext &C, ModuleNature MN,
                         const IdentifierInfo *Info,
                         Token StmtLabelTok) {
  return new (C) UseStmt(MN, Info, StmtLabelTok);
}

llvm::StringRef UseStmt::getModuleName() const {
  return ModName->getName();
}

void UseStmt::addRenameItem(const IdentifierInfo *LocalName,
                            const IdentifierInfo *UseName) {
  RenameList.push_back(std::make_pair(LocalName, UseName));
}

void UseStmt::addRenameItem(const IdentifierInfo *UseName) {
  RenameList.push_back(std::make_pair(UseName, UseName));
}

//===----------------------------------------------------------------------===//
// Import Statement
//===----------------------------------------------------------------------===//

ImportStmt::ImportStmt(Token StmtLabelTok)
  : Stmt(Import, SMLoc(), StmtLabelTok) {
}
ImportStmt::ImportStmt(ArrayRef<const IdentifierInfo*> names,
                       Token StmtLabelTok)
  : Stmt(Import, SMLoc(), StmtLabelTok) {
  Names.resize(names.size());
  std::copy(names.begin(), names.end(), Names.begin());
}

ImportStmt *ImportStmt::Create(ASTContext &C, Token StmtLabelTok) {
  return new (C) ImportStmt(StmtLabelTok);
}

ImportStmt *ImportStmt::Create(ASTContext &C,
                               ArrayRef<const IdentifierInfo*> Names,
                               Token StmtLabelTok) {
  return new (C) ImportStmt(Names, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// Implicit Statement
//===----------------------------------------------------------------------===//

ImplicitStmt::ImplicitStmt(SMLoc L, Token StmtLabel)
  : Stmt(Implicit, L, StmtLabel), None(true) {}

ImplicitStmt *ImplicitStmt::Create(ASTContext &C, SMLoc L, Token StmtLabel) {
  return new (C) ImplicitStmt(L, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Asynchronous Statement
//===----------------------------------------------------------------------===//

AsynchronousStmt::
AsynchronousStmt(llvm::ArrayRef<const IdentifierInfo*> objNames,
                 Token StmtLabelTok)
  : Stmt(Asynchronous, llvm::SMLoc(), StmtLabelTok) {
  std::copy(objNames.begin(), objNames.end(), ObjNames.begin());
}

AsynchronousStmt *AsynchronousStmt::
Create(ASTContext &C, llvm::ArrayRef<const IdentifierInfo*> objNames,
       Token StmtLabelTok) {
  return new (C) AsynchronousStmt(objNames, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// Assignment Statement
//===----------------------------------------------------------------------===//

AssignmentStmt::AssignmentStmt(ExprResult lhs, ExprResult rhs,
                               Token StmtLabelTok)
  : Stmt(Assignment, llvm::SMLoc(), StmtLabelTok), LHS(lhs), RHS(rhs)
{}

AssignmentStmt *AssignmentStmt::Create(ASTContext &C, ExprResult lhs,
                                       ExprResult rhs, Token StmtLabelTok) {
  return new (C) AssignmentStmt(lhs, rhs, StmtLabelTok);
}

//===----------------------------------------------------------------------===//
// Print Statement
//===----------------------------------------------------------------------===//

PrintStmt::PrintStmt(SMLoc L, FormatSpec *fs, ArrayRef<ExprResult> OutList,
                     Token StmtLabelTok)
  : Stmt(Print, L, StmtLabelTok), FS(fs) {
  OutputItemList.append(OutList.begin(), OutList.end());
}

PrintStmt *PrintStmt::Create(ASTContext &C, SMLoc L, FormatSpec *fs,
                             ArrayRef<ExprResult> OutList, Token StmtLabelTok) {
  return new (C) PrintStmt(L, fs, OutList, StmtLabelTok);
}
