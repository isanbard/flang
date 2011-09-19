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

StarFormatSpec::StarFormatSpec(SMLoc Loc)
  : FormatSpec(FormatSpec::FS_Star, Loc) {}

StarFormatSpec *StarFormatSpec::Create(ASTContext &C, SMLoc Loc) {
  return new (C) StarFormatSpec(Loc);
}

DefaultCharFormatSpec::DefaultCharFormatSpec(SMLoc L, ExprResult F)
  : FormatSpec(FormatSpec::FS_DefaultCharExpr, L), Fmt(F) {}

DefaultCharFormatSpec *DefaultCharFormatSpec::Create(ASTContext &C, SMLoc Loc,
                                                   ExprResult Fmt) {
  return new (C) DefaultCharFormatSpec(Loc, Fmt);
}

LabelFormatSpec::LabelFormatSpec(SMLoc L, ExprResult Lbl)
  : FormatSpec(FormatSpec::FS_Label, L), Label(Lbl) {}

LabelFormatSpec *LabelFormatSpec::Create(ASTContext &C, SMLoc Loc,
                                         ExprResult Label) {
  return new (C) LabelFormatSpec(Loc, Label);
}
