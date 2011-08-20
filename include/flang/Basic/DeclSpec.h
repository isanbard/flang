//===-- DeclSpec.h - Declaration Type Specifiers ----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Declaration type specifiers.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_DECL_SPEC_H__
#define FORTRAN_DECL_SPEC_H__

#include "flang/Sema/Ownership.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
template <typename T> class ArrayRef;
class raw_ostream;
} // end llvm namespace

namespace fortran {

class Type;
class Expr;

//===----------------------------------------------------------------------===//
/// DeclSpec - A declaration type specifier is the type -- intrinsic, TYPE, or
/// CLASS -- plus any kind selectors for that type.
class DeclSpec {
public:
  enum TypeSpec {
    dts_None,
    dts_IntrinsicTypeSpec,
    dts_DerivedTypeSpec
  };
private:
  TypeSpec TS;
public:
  explicit DeclSpec(TypeSpec ts) : TS(ts) {}
  virtual ~DeclSpec();

  TypeSpec getClassID() const { return TS; }

  virtual void print(llvm::raw_ostream &) {}

  static bool classof(DeclSpec*) { return true; }
};

//===----------------------------------------------------------------------===//
/// IntrinsicDeclSpec - A declaration type specifier for an intrinsic type.
class IntrinsicDeclSpec : public DeclSpec {
  const Type *Ty;
public:
  IntrinsicDeclSpec(const Type *T)
    : DeclSpec(dts_IntrinsicTypeSpec), Ty(T) {}
  virtual ~IntrinsicDeclSpec();

  const Type *getType() const { return Ty; }
  void setType(const Type *ty) { Ty = ty; }

  virtual void print(llvm::raw_ostream &);

  static bool classof(DeclSpec *DTS) {
    return DTS->getClassID() == dts_IntrinsicTypeSpec;
  }
  static bool classof(IntrinsicDeclSpec*) { return true; }
};

//===----------------------------------------------------------------------===//
/// DerivedDeclSpec - A declaration type specifier for a derived type.
class DerivedDeclSpec : public DeclSpec {
  ExprResult TypeExpr;
  llvm::SmallVector<ExprResult, 4> TypeParamSpec;
public:
  DerivedDeclSpec(ExprResult E, llvm::ArrayRef<ExprResult> Arr);
  virtual ~DerivedDeclSpec();

  const ExprResult getTypeExpr() const { return TypeExpr; }
  void setTypeExpr(ExprResult E) { TypeExpr = E; }

  virtual void print(llvm::raw_ostream &);

  static bool classof(DeclSpec *DTS) {
    return DTS->getClassID() == dts_DerivedTypeSpec;
  }
  static bool classof(DerivedDeclSpec*) { return true; }
};

} // end fortran namespace

#endif
