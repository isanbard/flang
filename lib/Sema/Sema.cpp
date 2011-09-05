//===--- Sema.cpp - AST Builder and Semantic Analysis Implementation ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the actions class which performs semantic analysis and
// builds an AST out of a parse stream.
//
//===----------------------------------------------------------------------===//

#include "flang/Sema/Sema.h"
#include "flang/AST/ASTContext.h"
#include "flang/AST/Decl.h"
using namespace flang;

Sema::Sema(ASTContext &ctxt, llvm::SourceMgr &sm)
  : Context(ctxt), SrcMgr(sm), CurContext(0) {}

Sema::~Sema() {}

// getContainingDC - Determines the context to return to after temporarily
// entering a context.  This depends in an unnecessarily complicated way on the
// exact ordering of callbacks from the parser.
DeclContext *Sema::getContainingDC(DeclContext *DC) {
  return DC->getParent();
}

void Sema::PushDeclContext(DeclContext *DC) {
  assert(getContainingDC(DC) == CurContext &&
      "The next DeclContext should be lexically contained in the current one.");
  CurContext = DC;
}

void Sema::PopDeclContext() {
  assert(CurContext && "DeclContext imbalance!");

  CurContext = getContainingDC(CurContext);
  assert(CurContext && "Popped translation unit!");
}

void Sema::ActOnMainProgram() {
  PushDeclContext(Context.getMainProgramDecl());
}
