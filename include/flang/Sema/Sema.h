//===--- Sema.h - Semantic Analysis & AST Building --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Sema class, which performs semantic analysis and builds
// ASTs.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_SEMA_SEMA_H__
#define FLANG_SEMA_SEMA_H__

#include "flang/Sema/Ownership.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SMLoc.h"

namespace flang {

class ASTContext;
class DeclContext;
class DeclarationNameInfo;
class Expr;
class IdentifierInfo;
class Token;

/// Sema - This implements semantic analysis and AST buiding for Fortran.
class Sema {
  Sema(const Sema&);           // DO NOT IMPLEMENT
  void operator=(const Sema&); // DO NOT IMPLEMENT
public:
  typedef Expr ExprTy;

  ASTContext &Context;
  llvm::SourceMgr &SrcMgr;

  /// CurContext - This is the current declaration context of parsing.
  DeclContext *CurContext;

  Sema(ASTContext &ctxt, llvm::SourceMgr &sm);
  ~Sema();

  DeclContext *getContainingDC(DeclContext *DC);

  /// Set the current declaration context until it gets popped.
  void PushDeclContext(DeclContext *DC);
  void PopDeclContext();

  void ActOnTranslationUnit();
  void ActOnMainProgram(const DeclarationNameInfo &NameInfo);
  void ActOnEndProgramUnit();


  StmtResult ActOnPROGRAM(ASTContext &C, const IdentifierInfo *ProgName,
                          llvm::SMLoc Loc, llvm::SMLoc NameLoc,
                          Token &StmtLabelTok);
};

} // end flang namespace

#endif
