//===-- Type.cpp - Fortran Type Interface ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The Fortran type interface.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/Type.h"
#include "flang/AST/ASTContext.h"
#include "flang/AST/Expr.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
using namespace fortran;

//===----------------------------------------------------------------------===//
//                          Anchor the Type class
//===----------------------------------------------------------------------===//

Type::~Type() {}

void Type::Destroy(ASTContext &C) {
  this->~Type();
  C.Deallocate(this);
}

//===----------------------------------------------------------------------===//
//                             Subtype Methods
//===----------------------------------------------------------------------===//

BuiltinType::~BuiltinType() {
  delete Kind.getKindExpr().take();
}

void BuiltinType::print(llvm::raw_ostream &O) const {
  switch (getTypeSpec()) {
  default: assert(false && "Invalid built-in type!");
  case BuiltinType::TS_Integer:
    O << "integer";
    break;
  case BuiltinType::TS_Real:
    O << "real";
    break;
  case BuiltinType::TS_DoublePrecision:
    O << "double_precision";
    break;
  case BuiltinType::TS_Complex:
    O << "complex";
    break;
  case BuiltinType::TS_Logical:
    O << "logical";
    break;
  }

  if (Kind.getKindExpr().isUsable()) {
    O << " kind=\"";
    Kind.print(O);
    O << "\"";
  }
}

CharacterBuiltinType::~CharacterBuiltinType() {
  delete Len.getKindExpr().take();
}

void CharacterBuiltinType::print(llvm::raw_ostream &O) const {
  O << "character";

  if (Len.getKindExpr().isUsable()) {
    O << " length=\"";
    Len.print(O);
    O << "\"";
  }

  if (Kind.getKindExpr().isUsable()) {
    O << " kind=\"";
    Kind.print(O);
    O << "\"";
  }
}

//===----------------------------------------------------------------------===//
//                             Selector Methods
//===----------------------------------------------------------------------===//

void Selector::print(llvm::raw_ostream &O) const {
  KindExpr.get()->print(O);
}
