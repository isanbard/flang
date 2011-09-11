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
  if (const Type *Ty = Value.getPointer().dyn_cast<const Type*>()) {
    if (const BuiltinType *BTy = dyn_cast<BuiltinType>(Ty))
      BTy->print(OS);
    return;
  }

  const ExtQuals *EQ = Value.getPointer().get<const ExtQuals*>();
  if (const BuiltinType *BTy = dyn_cast<BuiltinType>(EQ->BaseType))
    BTy->print(OS);

  bool Comma = false;
  if (EQ->hasKindSelector()) {
    OS << " (KIND=";
    EQ->getKindSelector()->print(OS);
    if (EQ->hasLengthSelector()) {
      OS << ", LEN=";
      EQ->getLengthSelector()->print(OS);
    }
    OS << ")";
    Comma = true;
  } else if (EQ->hasLengthSelector()) {
    OS << " (LEN=";
    EQ->getLengthSelector()->print(OS);
    OS << ")";
    Comma = true;
  }

#define PRINT_QUAL(Q, QNAME) \
  do {                                                      \
    if (Quals.hasAttributeSpec(Qualifiers::Q)) {            \
      if (Comma) OS << ", "; Comma = true;                  \
      OS << QNAME;                                          \
    }                                                       \
  } while (0)

  Qualifiers Quals = EQ->getQualifiers();
  PRINT_QUAL(AS_allocatable,  "ALLOCATABLE");
  PRINT_QUAL(AS_asynchronous, "ASYNCHRONOUS");
  PRINT_QUAL(AS_codimension,  "CODIMENSION");
  PRINT_QUAL(AS_contiguous,   "CONTIGUOUS");
  PRINT_QUAL(AS_dimension,    "DIMENSION");
  PRINT_QUAL(AS_external,     "EXTERNAL");
  PRINT_QUAL(AS_intrinsic,    "INTRINSIC");
  PRINT_QUAL(AS_optional,     "OPTIONAL");
  PRINT_QUAL(AS_parameter,    "PARAMETER");
  PRINT_QUAL(AS_pointer,      "POINTER");
  PRINT_QUAL(AS_protected,    "PROTECTED");
  PRINT_QUAL(AS_save,         "SAVE");
  PRINT_QUAL(AS_target,       "TARGET");
  PRINT_QUAL(AS_value,        "VALUE");
  PRINT_QUAL(AS_volatile,     "VOLATILE");
}
