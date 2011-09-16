//===--- FormatSpec.h - Fortran Format Specifier ----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the format specifier class, used by the PRINT statement,
//  et al.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_AST_FORMATSPEC_H__
#define FLANG_AST_FORMATSPEC_H__

#include "llvm/Support/SMLoc.h"
#include "flang/Basic/LLVM.h"

namespace flang {

class ASTContext;

class FormatSpec {
public:
  enum FormatType { DefaultCharExpr, Label, Star };
private:
  FormatType Ty;
  SMLoc Loc;
  FormatSpec(FormatType T, SMLoc L);
  friend class ASTContext;
public:
  static FormatSpec *Create(ASTContext &C, FormatType Ty, SMLoc Loc);

  FormatType getType() const { return Ty; }
};

} // end namespace flang

#endif
