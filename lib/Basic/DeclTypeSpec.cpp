//===-- DeclTypeSpec.cpp - Fortran Declaration Type Specifier Interface ---===//
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

#include "flang/Basic/DeclTypeSpec.h"
#include "flang/AST/Expr.h"
#include "flang/Basic/Type.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/ArrayRef.h"
using namespace fortran;

//===----------------------------------------------------------------------===//
// Declaration Type Specifier
//===----------------------------------------------------------------------===//

DeclTypeSpec::~DeclTypeSpec() {}

//===----------------------------------------------------------------------===//
// Intrinsic Declaration Type Specifier
//===----------------------------------------------------------------------===//

IntrinsicDeclTypeSpec::~IntrinsicDeclTypeSpec() {}

void IntrinsicDeclTypeSpec::print(llvm::raw_ostream &O) {
  Ty->print(O);
}

//===----------------------------------------------------------------------===//
// Derived Declaration Type Specifier
//===----------------------------------------------------------------------===//

DerivedDeclTypeSpec::DerivedDeclTypeSpec(ExprResult e,
                                         llvm::ArrayRef<ExprResult> Arr)
  : DeclTypeSpec(dts_DerivedTypeSpec), TypeExpr(e) {
  for (unsigned I = 0, N = Arr.size(); I != N; ++I)
    TypeParamSpec.push_back(Arr[I]);
}

DerivedDeclTypeSpec::~DerivedDeclTypeSpec() {
  delete TypeExpr.take();
}

void DerivedDeclTypeSpec::print(llvm::raw_ostream &O) {
  TypeExpr.get()->print(O);
}
