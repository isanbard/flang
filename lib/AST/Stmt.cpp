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

UseStmt::UseStmt(ASTContext &C, ModuleNature MN, const IdentifierInfo *modName,
                 ArrayRef<RenamePair> RenameList, ExprResult StmtLabel)
  : ListStmt(C, Use, SMLoc(), RenameList, StmtLabel),
    ModNature(MN), ModName(modName), Only(false) {}

UseStmt *UseStmt::Create(ASTContext &C, ModuleNature MN,
                         const IdentifierInfo *modName,
                         ExprResult StmtLabel) {
  return  new (C) UseStmt(C, MN, modName, ArrayRef<RenamePair>(), StmtLabel);
}

UseStmt *UseStmt::Create(ASTContext &C, ModuleNature MN,
                         const IdentifierInfo *modName, bool Only,
                         ArrayRef<RenamePair> RenameList,
                         ExprResult StmtLabel) {
  UseStmt *US = new (C) UseStmt(C, MN, modName, RenameList, StmtLabel);
  US->Only = Only;
  return US;
}

llvm::StringRef UseStmt::getModuleName() const {
  return ModName->getName();
}

//===----------------------------------------------------------------------===//
// Import Statement
//===----------------------------------------------------------------------===//

ImportStmt::ImportStmt(ASTContext &C, SMLoc Loc,
                       ArrayRef<const IdentifierInfo*> Names,
                       ExprResult StmtLabel)
  : ListStmt(C, Import, Loc, Names, StmtLabel) {}

ImportStmt *ImportStmt::Create(ASTContext &C, SMLoc Loc,
                               ArrayRef<const IdentifierInfo*> Names,
                               ExprResult StmtLabel) {
  return new (C) ImportStmt(C, Loc, Names, StmtLabel);
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
  : Stmt(Implicit, L, StmtLabel), Ty(T), None(false) {
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

ParameterStmt::ParameterStmt(ASTContext &C, SMLoc Loc,
                             ArrayRef<ParamPair> PList, ExprResult StmtLabel)
  : Stmt(Parameter, Loc, StmtLabel) {
  NumParams = PList.size();
  ParamList = new (C) ParamPair[NumParams];

  for (unsigned I = 0; I != NumParams; ++I)
    ParamList[I] = PList[I];
}

ParameterStmt *ParameterStmt::Create(ASTContext &C, SMLoc Loc,
                                     ArrayRef<ParamPair>ParamList,
                                     ExprResult StmtLabel) {
  return new (C) ParameterStmt(C, Loc, ParamList, StmtLabel);
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
// Entry Statement
//===----------------------------------------------------------------------===//

EntryStmt::EntryStmt(SMLoc Loc, ExprResult StmtLabel)
  : Stmt(Entry, Loc, StmtLabel) {}

EntryStmt *EntryStmt::Create(ASTContext &C, SMLoc Loc, ExprResult StmtLabel) {
  return new (C) EntryStmt(Loc, StmtLabel);
}

//===----------------------------------------------------------------------===//
// Asynchronous Statement
//===----------------------------------------------------------------------===//

AsynchronousStmt::
AsynchronousStmt(ASTContext &C, SMLoc Loc,
                 ArrayRef<const IdentifierInfo*> objNames,
                 ExprResult StmtLabel)
  : ListStmt(C, Asynchronous, Loc, objNames, StmtLabel) {}

AsynchronousStmt *AsynchronousStmt::
Create(ASTContext &C, SMLoc Loc, ArrayRef<const IdentifierInfo*> objNames,
       ExprResult StmtLabel) {
  return new (C) AsynchronousStmt(C, Loc, objNames, StmtLabel);
}

//===----------------------------------------------------------------------===//
// External Statement
//===----------------------------------------------------------------------===//

ExternalStmt::ExternalStmt(ASTContext &C, SMLoc Loc,
                           ArrayRef<const IdentifierInfo *> ExternalNames,
                           ExprResult StmtLabel)
  : ListStmt(C, External, Loc, ExternalNames, StmtLabel) {}

ExternalStmt *ExternalStmt::Create(ASTContext &C, SMLoc Loc,
                                   ArrayRef<const IdentifierInfo*>ExternalNames,
                                   ExprResult StmtLabel) {
  return new (C) ExternalStmt(C, Loc, ExternalNames, StmtLabel);
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
