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
    None,
    IntrinsicTypeSpec,
    DerivedTypeSpec
  };

  enum AttrSpec {
    AS_None            = 0,
    AS_Allocatable     = 1 << 0,
    AS_Asynchronous    = 1 << 1,
    AS_Codimension     = 1 << 2,
    AS_Contiguous      = 1 << 3,
    AS_Dimension       = 1 << 4,
    AS_External        = 1 << 5,
    AS_Intent          = 1 << 6,
    AS_Intrinsic       = 1 << 7,
    AS_Optional        = 1 << 8,
    AS_Parameter       = 1 << 9,
    AS_Pointer         = 1 << 10,
    AS_Protected       = 1 << 11,
    AS_Save            = 1 << 12,
    AS_Target          = 1 << 13,
    AS_Value           = 1 << 14,
    AS_Volatile        = 1 << 15,
    AS_AccessSpec      = 1 << 30,
    AS_LangBindingSpec = 1 << 31
  };

  enum AccessSpec {
    AC_None    = 0,
    AC_Public  = 1 << 0,
    AC_Private = 1 << 1
  };

  enum IntentSpec {
    IS_None   = 0,
    IS_In     = 1 << 0,
    IS_Out    = 1 << 1,
    IS_InOut  = 1 << 2
  };

private:
  TypeSpec TS;                 //< Type specifier: REAL, INTEGER, etc.
  uint32_t AS;                 //< Attribute specifier: PARAMETER, POINTER, etc.
  uint16_t AC;                 //< Access control specifier: PUBLIC, PRIVATE
  uint16_t IS;                 //< Intent specifier: IN, OUT, INOUT
public:
  explicit DeclSpec(TypeSpec ts) : TS(ts) {}
  virtual ~DeclSpec();

  TypeSpec getClassID() const { return TS; }

  // Accessors functions.
  bool hasAttribute(AttrSpec Val) const {  return (AS & Val) != 0; }
  void setAttribute(AttrSpec Val) { AS |= Val; }

  bool hasAccessControl(AccessSpec Val) const {  return (AC & Val) != 0; }
  void setAccessControl(AccessSpec Val) { AC |= Val; }

  bool hasIntent(IntentSpec Val) const {  return (IS & Val) != 0; }
  void setIntent(IntentSpec Val) { IS |= Val; }

  virtual void print(llvm::raw_ostream &) {}

  static bool classof(DeclSpec*) { return true; }
};

//===----------------------------------------------------------------------===//
/// IntrinsicDeclSpec - A declaration type specifier for an intrinsic type.
class IntrinsicDeclSpec : public DeclSpec {
  const Type *Ty;
public:
  IntrinsicDeclSpec(const Type *T)
    : DeclSpec(IntrinsicTypeSpec), Ty(T) {}
  virtual ~IntrinsicDeclSpec();

  const Type *getType() const { return Ty; }
  void setType(const Type *ty) { Ty = ty; }

  virtual void print(llvm::raw_ostream &);

  static bool classof(DeclSpec *DTS) {
    return DTS->getClassID() == IntrinsicTypeSpec;
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
    return DTS->getClassID() == DerivedTypeSpec;
  }
  static bool classof(DerivedDeclSpec*) { return true; }
};

} // end fortran namespace

#endif
