//===-- DeclTypeSpec.h - Declaration Type Specifiers ------------*- C++ -*-===//
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

#ifndef FORTRAN_DECL_TYPE_SPEC_H__
#define FORTRAN_DECL_TYPE_SPEC_H__

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
/// DeclTypeSpec - A declaration type specifier is the type -- intrinsic, TYPE,
/// or CLASS -- plus any kind selectors for that type.
class DeclTypeSpec {
public:
  enum TypeSpec {
    dts_None,
    dts_IntrinsicTypeSpec,
    dts_DerivedTypeSpec
  };
private:
  TypeSpec TS;
public:
  explicit DeclTypeSpec(TypeSpec ts) : TS(ts) {}
  virtual ~DeclTypeSpec();

  TypeSpec getClassID() const { return TS; }

  virtual void print(llvm::raw_ostream &) {}

  static bool classof(DeclTypeSpec*) { return true; }
};

//===----------------------------------------------------------------------===//
/// IntrinsicDeclTypeSpec - A declaration type specifier for an intrinsic type.
class IntrinsicDeclTypeSpec : public DeclTypeSpec {
  const Type *Ty;
public:
  IntrinsicDeclTypeSpec(const Type *T)
    : DeclTypeSpec(dts_IntrinsicTypeSpec), Ty(T) {}
  virtual ~IntrinsicDeclTypeSpec();

  const Type *getType() const { return Ty; }
  void setType(const Type *ty) { Ty = ty; }

  virtual void print(llvm::raw_ostream &);

  static bool classof(DeclTypeSpec *DTS) {
    return DTS->getClassID() == dts_IntrinsicTypeSpec;
  }
  static bool classof(IntrinsicDeclTypeSpec*) { return true; }
};

//===----------------------------------------------------------------------===//
/// DerivedDeclTypeSpec - A declaration type specifier for a derived type.
class DerivedDeclTypeSpec : public DeclTypeSpec {
  ExprResult TypeExpr;
  llvm::SmallVector<ExprResult, 4> TypeParamSpec;
public:
  DerivedDeclTypeSpec(ExprResult E, llvm::ArrayRef<ExprResult> Arr);
  virtual ~DerivedDeclTypeSpec();

  const ExprResult getTypeExpr() const { return TypeExpr; }
  void setTypeExpr(ExprResult E) { TypeExpr = E; }

  virtual void print(llvm::raw_ostream &);

  static bool classof(DeclTypeSpec *DTS) {
    return DTS->getClassID() == dts_DerivedTypeSpec;
  }
  static bool classof(DerivedDeclTypeSpec*) { return true; }
};

} // end fortran namespace

#endif
