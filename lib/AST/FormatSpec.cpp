//===--- FormatSpec.cpp - Fortran FormatSpecifier -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/FormatSpec.h"
#include "flang/AST/ASTContext.h"
using namespace flang;

FormatSpec::FormatSpec(FormatType T, SMLoc L)
  : Ty(T), Loc(L) {}

FormatSpec *FormatSpec::Create(ASTContext &C, FormatType Ty, SMLoc Loc) {
  return new (C) FormatSpec(Ty, Loc);
}
