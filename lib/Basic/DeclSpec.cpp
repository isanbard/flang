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

#include "flang/Basic/DeclSpec.h"
#include "flang/AST/Expr.h"
#include "flang/AST/Type.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/ArrayRef.h"
using namespace fortran;

//===----------------------------------------------------------------------===//
// Declaration Type Specifier
//===----------------------------------------------------------------------===//

DeclSpec::~DeclSpec() {}

//===----------------------------------------------------------------------===//
// Intrinsic Declaration Type Specifier
//===----------------------------------------------------------------------===//

IntrinsicDeclSpec::~IntrinsicDeclSpec() {}

void IntrinsicDeclSpec::print(llvm::raw_ostream &O) {
  Ty->print(O);
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
