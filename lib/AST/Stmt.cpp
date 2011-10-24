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
                                 ExprResult StmtLabel) {
  return new (C) ProgramStmt(ProgName, Loc, NameLoc, StmtLabel);
}

//===----------------------------------------------------------------------===//
// EndProgram Statement
//===----------------------------------------------------------------------===//

EndProgramStmt *EndProgramStmt::Create(ASTContext &C,
                                       const IdentifierInfo *ProgName,
                                       llvm::SMLoc Loc, llvm::SMLoc NameLoc,
                                       ExprResult StmtLabel) {
  return new (C) EndProgramStmt(ProgName, Loc, NameLoc, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Use Statement
//===----------------------------------------------------------------------===//

UseStmt::UseStmt(ModuleNature MN, const IdentifierInfo *Info,
                 ExprResult StmtLabel)
  : Stmt(Use, llvm::SMLoc(), StmtLabel), ModNature(MN), ModName(Info) {}

UseStmt *UseStmt::Create(ASTContext &C, ModuleNature MN,
                         const IdentifierInfo *Info,
                         ExprResult StmtLabel) {
  return new (C) UseStmt(MN, Info, StmtLabel);
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

ImportStmt::ImportStmt(ExprResult StmtLabel)
  : Stmt(Import, SMLoc(), StmtLabel) {
}
ImportStmt::ImportStmt(ArrayRef<const IdentifierInfo*> names,
                       ExprResult StmtLabel)
  : Stmt(Import, SMLoc(), StmtLabel) {
  Names.resize(names.size());
  std::copy(names.begin(), names.end(), Names.begin());
}

ImportStmt *ImportStmt::Create(ASTContext &C, ExprResult StmtLabel) {
  return new (C) ImportStmt(StmtLabel);
}

ImportStmt *ImportStmt::Create(ASTContext &C,
                               ArrayRef<const IdentifierInfo*> Names,
                               ExprResult StmtLabel) {
  return new (C) ImportStmt(Names, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Implicit Statement
//===----------------------------------------------------------------------===//

ImplicitStmt::ImplicitStmt(SMLoc L, ExprResult StmtLabel)
  : Stmt(Implicit, L, StmtLabel), None(true) {}

ImplicitStmt *ImplicitStmt::Create(ASTContext &C, SMLoc L,
                                   ExprResult StmtLabel) {
  return new (C) ImplicitStmt(L, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Asynchronous Statement
//===----------------------------------------------------------------------===//

AsynchronousStmt::
AsynchronousStmt(llvm::ArrayRef<const IdentifierInfo*> objNames,
                 ExprResult StmtLabel)
  : Stmt(Asynchronous, llvm::SMLoc(), StmtLabel) {
  std::copy(objNames.begin(), objNames.end(), ObjNames.begin());
}

AsynchronousStmt *AsynchronousStmt::
Create(ASTContext &C, llvm::ArrayRef<const IdentifierInfo*> objNames,
       ExprResult StmtLabel) {
  return new (C) AsynchronousStmt(objNames, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Assignment Statement
//===----------------------------------------------------------------------===//

AssignmentStmt::AssignmentStmt(ExprResult lhs, ExprResult rhs,
                               ExprResult StmtLabel)
  : Stmt(Assignment, llvm::SMLoc(), StmtLabel), LHS(lhs), RHS(rhs)
{}

AssignmentStmt *AssignmentStmt::Create(ASTContext &C, ExprResult lhs,
                                       ExprResult rhs, ExprResult StmtLabel) {
  return new (C) AssignmentStmt(lhs, rhs, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Print Statement
//===----------------------------------------------------------------------===//

PrintStmt::PrintStmt(SMLoc L, FormatSpec *fs, ArrayRef<ExprResult> OutList,
                     ExprResult StmtLabel)
  : Stmt(Print, L, StmtLabel), FS(fs) {
  OutputItemList.append(OutList.begin(), OutList.end());
}

PrintStmt *PrintStmt::Create(ASTContext &C, SMLoc L, FormatSpec *fs,
                             ArrayRef<ExprResult> OutList,
                             ExprResult StmtLabel) {
  return new (C) PrintStmt(L, fs, OutList, StmtLabel);
}
