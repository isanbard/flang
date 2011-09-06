//===-- DeclSpec.cpp - Fortran Declaration Type Specifier Interface ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The Fortran declaration type specifier interface.
//
//===----------------------------------------------------------------------===//

#include "flang/Sema/DeclSpec.h"
#include "flang/AST/Expr.h"
#include "flang/AST/Type.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/ArrayRef.h"
using namespace flang;

//===----------------------------------------------------------------------===//
// Declaration Type Specifier
//===----------------------------------------------------------------------===//

DeclSpec::~DeclSpec() {}

//===----------------------------------------------------------------------===//
// Intrinsic Declaration Type Specifier
//===----------------------------------------------------------------------===//

IntrinsicDeclSpec::~IntrinsicDeclSpec() {}

void IntrinsicDeclSpec::print(llvm::raw_ostream &O) {
  const Type *T = Ty.getTypePtr();
  if (const CharacterBuiltinType *CT = dyn_cast<CharacterBuiltinType>(T))
    CT->print(O);
  else if (const BuiltinType *BT = dyn_cast<BuiltinType>(T))
    BT->print(O);
  else if (const PointerType *PT = dyn_cast<PointerType>(T))
    PT->print(O);
  else if (const ArrayType *AT = dyn_cast<ArrayType>(T))
    AT->print(O);
  else if (const RecordType *RT = dyn_cast<RecordType>(T))
    RT->print(O);
}

//===----------------------------------------------------------------------===//
// Derived Declaration Type Specifier
//===----------------------------------------------------------------------===//

DerivedDeclSpec::DerivedDeclSpec(ExprResult e,
                                 llvm::ArrayRef<ExprResult> Arr)
  : DeclSpec(DerivedTypeSpec), TypeExpr(e) {
  for (unsigned I = 0, N = Arr.size(); I != N; ++I)
    TypeParamSpec.push_back(Arr[I]);
}

DerivedDeclSpec::~DerivedDeclSpec() {
  delete TypeExpr.take();
}

void DerivedDeclSpec::print(llvm::raw_ostream &O) {
  TypeExpr.get()->print(O);
}
