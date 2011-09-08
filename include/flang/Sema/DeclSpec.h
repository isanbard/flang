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

#ifndef FLANG_SEMA_DECLSPEC_H__
#define FLANG_SEMA_DECLSPEC_H__

#include "flang/AST/Type.h"
#include "flang/Basic/Specifiers.h"
#include "flang/Sema/Ownership.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
  template <typename T> class ArrayRef;
  class raw_ostream;
} // end llvm namespace

namespace flang {

class Expr;

//===----------------------------------------------------------------------===//
/// DeclSpec - A declaration type specifier is the type -- intrinsic, TYPE, or
/// CLASS -- plus any kind selectors for that type.
class DeclSpec {
public:
  // Import intrinsic type specifiers.
  typedef TypeSpecifierType TST;
  static const TST TST_unspecified = flang::TST_unspecified;
  static const TST TST_integer = flang::TST_integer;
  static const TST TST_real = flang::TST_real;
  static const TST TST_doubleprecision = flang::TST_doubleprecision;
  static const TST TST_complex = flang::TST_complex;
  static const TST TST_character = flang::TST_character;
  static const TST TST_logical = flang::TST_logical;
  static const TST TST_struct = flang::TST_struct;

  // Import attribute specifiers.
  typedef AttributeSpecifier AS;
  static const AS AS_unspecified = flang::AS_unspecified;
  static const AS AS_allocatable = flang::AS_allocatable;
  static const AS AS_asynchronous = flang::AS_asynchronous;
  static const AS AS_codimension = flang::AS_codimension;
  static const AS AS_contiguous = flang::AS_contiguous;
  static const AS AS_dimension = flang::AS_dimension;
  static const AS AS_external = flang::AS_external;
  static const AS AS_intrinsic = flang::AS_intrinsic;
  static const AS AS_optional = flang::AS_optional;
  static const AS AS_parameter = flang::AS_parameter;
  static const AS AS_pointer = flang::AS_pointer;
  static const AS AS_protected = flang::AS_protected;
  static const AS AS_save = flang::AS_save;
  static const AS AS_target = flang::AS_target;
  static const AS AS_value = flang::AS_value;
  static const AS AS_volatile = flang::AS_volatile;

  /// Import intent specifiers.
  typedef IntentSpecifier IS;
  static const IS IS_unspecified = flang::IS_unspecified;
  static const IS IS_in = flang::IS_in;
  static const IS IS_out = flang::IS_out;
  static const IS IS_inout = flang::IS_inout;

  /// Import access specifiers.
  typedef AccessSpecifier AC;
  static const AC AC_unspecified = flang::AC_unspecified;
  static const AC AC_public = flang::AC_public;
  static const AC AC_private = flang::AC_private;

  enum TQ { // NOTE: These flags must be kept in sync with Qualifiers::TQ.
    TQ_unspecified = 0,
    TQ_allocatable = 1 << 0,
    TQ_parameter   = 1 << 1,
    TQ_volatile    = 1 << 2
  };

  enum TypeSpec {
    None,
    IntrinsicTypeSpec_,
    DerivedTypeSpec
  };

private:
  /*TST*/unsigned TypeSpecType   : 3;
  /*AS*/ unsigned AttributeSpecs : 15;
  /*TQ*/ unsigned TypeQualifiers : 3;  // Bitwise OR of TQ.
  /*IS*/ unsigned IntentSpec     : 3;
  /*AC*/ unsigned AccessSpec     : 3;

  Expr *Kind;                   // Kind Selector
  Expr *Len;                    // Length Selector

public:
  explicit DeclSpec()
    : TypeSpecType(TST_unspecified),
      AttributeSpecs(AS_unspecified),
      TypeQualifiers(TQ_unspecified),
      IntentSpec(IS_unspecified),
      AccessSpec(AC_unspecified),
      Kind(0), Len(0) {}
  virtual ~DeclSpec();

  bool hasKindSelector() const { return Kind != 0; }
  Expr *getKindSelector() const { return Kind; }
  void setKindSelector(Expr *K) { Kind = K; }

  bool hasLengthSelector() const { return Len != 0; }
  Expr *getLengthSelector() const { return Len; }
  void setLengthSelector(Expr *L) { Len = L; }

  /// getSpecifierName - Turn a type-specifier-type into a string like "REAL"
  /// or "ALLOCATABLE".
  static const char *getSpecifierName(DeclSpec::TQ Q);
  static const char *getSpecifierName(DeclSpec::TST I);
  static const char *getSpecifierName(DeclSpec::AS A);
  static const char *getSpecifierName(DeclSpec::IS I);

  bool hasTypeQual(DeclSpec::TQ Q) const {
    return TypeQualifiers & Q;
  }
  void setTypeQual(DeclSpec::TQ Q) {
    TypeQualifiers |= Q;
  }

  bool hasAttributeSpec(DeclSpec::AS A) const {
    return AttributeSpecs & A;
  }
  void setAttributeSpec(DeclSpec::AS A) {
    AttributeSpecs |= A;
  }
  unsigned getAttributeSpecs() const {
    return AttributeSpecs;
  }

  bool hasIntentSpec(DeclSpec::IS I) const {
    return IntentSpec & I;
  }
  IS getIntentSpec() const { return IS(IntentSpec); }
  void setIntentSpec(DeclSpec::IS I) {
    IntentSpec |= I;
  }

  bool hasAccessSpec(DeclSpec::AC A) const {
    return AccessSpec & A;
  }
  AC getAccessSpec() const { return AC(AccessSpec); }
  void setAccessSpec(DeclSpec::AC A) {
    AccessSpec |= A;
  }

  TST getTypeSpecType() const { return TST(TypeSpecType); }
  bool SetTypeSpecType(TST T) {
    if (TST(TypeSpecType) != TST_unspecified)
      return true;
    TypeSpecType = T;
    return false;
  }

  bool hasAttributes() const {
    return hasKindSelector() || hasLengthSelector() ||
      AttributeSpecs != 0 || IntentSpec != 0 || AccessSpec != 0;
  }

  TypeSpec getClassID() const { return None; } // FIXME: Remove

  virtual void print(llvm::raw_ostream &) {}

  static bool classof(DeclSpec*) { return true; }
};

} // end flang namespace

#endif
