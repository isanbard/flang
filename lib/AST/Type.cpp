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
using namespace flang;

QualType
QualifierCollector::apply(const ASTContext &Context, QualType QT) const {
  if (!hasNonFastQualifiers())
    return QT.withFastQualifiers(getFastQualifiers());

  return Context.getQualifiedType(QT, *this);
}

QualType
QualifierCollector::apply(const ASTContext &Context, const Type *T) const {
  if (!hasNonFastQualifiers())
    return QualType(T, getFastQualifiers());

  return Context.getQualifiedType(T, *this);
}

//===----------------------------------------------------------------------===//
//                             Subtype Methods
//===----------------------------------------------------------------------===//

void BuiltinType::print(llvm::raw_ostream &O) const {
  switch (getTypeSpec()) {
  default: assert(false && "Invalid built-in type!");
  case BuiltinType::Integer:
    O << "INTEGER";
    break;
  case BuiltinType::Real:
    O << "REAL";
    break;
  case BuiltinType::DoublePrecision:
    O << "DOUBLE PRECISION";
    break;
  case BuiltinType::Complex:
    O << "COMPLEX";
    break;
  case BuiltinType::Logical:
    O << "LOGICAL";
    break;
  }

  if (Kind) {
    O << " kind=\"";
    Kind->print(O);
    O << "\"";
  }
}

void CharacterBuiltinType::print(llvm::raw_ostream &O) const {
  O << "character";

  if (Len) {
    O << " length=\"";
    Len->print(O);
    O << "\"";
  }

  if (Kind) {
    O << " kind=\"";
    Kind->print(O);
    O << "\"";
  }
}

void QualType::dump() const {
  print(llvm::errs());
}

void QualType::print(raw_ostream &OS) const {
  if (const ExtQuals *EQ = Value.getPointer().dyn_cast<const ExtQuals*>()) {
    if (const BuiltinType *BTy = dyn_cast<BuiltinType>(EQ->BaseType))
      BTy->print(OS);
    return;
  }
  const Type *Ty = Value.getPointer().get<const Type*>();
  if (const BuiltinType *BTy = dyn_cast<BuiltinType>(Ty))
    BTy->print(OS);
}
