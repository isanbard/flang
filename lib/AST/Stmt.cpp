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

UseStmt::UseStmt(ASTContext &C, ModuleNature MN, const IdentifierInfo *Info,
                 ExprResult StmtLabel, ArrayRef<RenamePair> RenameLst)
  : Stmt(Use, llvm::SMLoc(), StmtLabel), ModNature(MN), ModName(Info) {
  NumRenames = RenameLst.size();
  RenameList = new (C) RenamePair[NumRenames];

  for (unsigned I = 0; I != NumRenames; ++I)
    RenameList[I] = RenameLst[I];
}

UseStmt *UseStmt::Create(ASTContext &C, ModuleNature MN,
                         const IdentifierInfo *Info,
                         ExprResult StmtLabel, ArrayRef<RenamePair> RenameList){
  return new (C) UseStmt(C, MN, Info, StmtLabel, RenameList);
}

llvm::StringRef UseStmt::getModuleName() const {
  return ModName->getName();
}

//===----------------------------------------------------------------------===//
// Import Statement
//===----------------------------------------------------------------------===//

ImportStmt::ImportStmt(ExprResult StmtLabel)
  : Stmt(Import, SMLoc(), StmtLabel), NumNames(0), Names(0) {
}
ImportStmt::ImportStmt(ASTContext &C, ArrayRef<const IdentifierInfo*> names,
                       ExprResult StmtLabel)
  : Stmt(Import, SMLoc(), StmtLabel), NumNames(names.size()), Names(0) {
  Names = new (C) const IdentifierInfo*[NumNames];

  for (unsigned I = 0; I != NumNames; ++I)
    Names[I] = names[I];
}

ImportStmt *ImportStmt::Create(ASTContext &C, ExprResult StmtLabel) {
  return new (C) ImportStmt(StmtLabel);
}

ImportStmt *ImportStmt::Create(ASTContext &C,
                               ArrayRef<const IdentifierInfo*> Names,
                               ExprResult StmtLabel) {
  return new (C) ImportStmt(C, Names, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Implicit Statement
//===----------------------------------------------------------------------===//

ImplicitStmt::ImplicitStmt(SMLoc L, ExprResult StmtLabel)
  : Stmt(Implicit, L, StmtLabel), None(true),
    NumLetterSpecs(0), LetterSpecList(0) {}

ImplicitStmt::ImplicitStmt(ASTContext &C, SMLoc L, QualType T,
                           ArrayRef<LetterSpec> SpecList,
                           ExprResult StmtLabel)
  : Stmt(Implicit, L, StmtLabel), None(false) {
  NumLetterSpecs = SpecList.size();
  LetterSpecList = new (C) LetterSpec[NumLetterSpecs];

  for (unsigned I = 0; I != NumLetterSpecs; ++I)
    LetterSpecList[I] = SpecList[I];
}

ImplicitStmt *ImplicitStmt::Create(ASTContext &C, SMLoc L,
                                   ExprResult StmtLabel) {
  return new (C) ImplicitStmt(L, StmtLabel);
}

ImplicitStmt *ImplicitStmt::Create(ASTContext &C, SMLoc L, QualType T,
                                   ArrayRef<LetterSpec> SpecList,
                                   ExprResult StmtLabel) {
  return new (C) ImplicitStmt(C, L, T, SpecList, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Parameter Statement
//===----------------------------------------------------------------------===//

ParameterStmt::ParameterStmt(SMLoc Loc, const IdentifierInfo *NC, ExprResult CE,
                             ExprResult StmtLabel)
  : Stmt(Parameter, Loc, StmtLabel), NamedConstant(NC), ConstantExpr(CE) {}

ParameterStmt *ParameterStmt::Create(ASTContext &C, SMLoc Loc,
                                     const IdentifierInfo *NC, ExprResult CE,
                                     ExprResult StmtLabel) {
  return new (C) ParameterStmt(Loc, NC, CE, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Format Statement
//===----------------------------------------------------------------------===//

FormatStmt::FormatStmt(SMLoc Loc, FormatSpec *fs, ExprResult StmtLabel)
  : Stmt(Format, Loc, StmtLabel), FS(fs) {}

FormatStmt *FormatStmt::Create(ASTContext &C, SMLoc Loc, FormatSpec *fs,
                               ExprResult StmtLabel) {
  return new (C) FormatStmt(Loc, fs, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Asynchronous Statement
//===----------------------------------------------------------------------===//

AsynchronousStmt::
AsynchronousStmt(ASTContext &C, ArrayRef<const IdentifierInfo*> objNames,
                 ExprResult StmtLabel)
  : Stmt(Asynchronous, llvm::SMLoc(), StmtLabel) {
  NumObjNames = objNames.size();
  ObjNames = new (C) const IdentifierInfo *[NumObjNames];

  for (unsigned I = 0; I != NumObjNames; ++I)
    ObjNames[I] = objNames[I];
}

AsynchronousStmt *AsynchronousStmt::
Create(ASTContext &C, ArrayRef<const IdentifierInfo*> objNames,
       ExprResult StmtLabel) {
  return new (C) AsynchronousStmt(C, objNames, StmtLabel);
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

PrintStmt::PrintStmt(ASTContext &C, SMLoc L, FormatSpec *fs,
                     ArrayRef<ExprResult> OutList, ExprResult StmtLabel)
  : Stmt(Print, L, StmtLabel), FS(fs) {
  NumOutputItems = OutList.size();
  OutputItemList = new (C) ExprResult[NumOutputItems];
}

PrintStmt *PrintStmt::Create(ASTContext &C, SMLoc L, FormatSpec *fs,
                             ArrayRef<ExprResult> OutList,
                             ExprResult StmtLabel) {
  return new (C) PrintStmt(C, L, fs, OutList, StmtLabel);
}
