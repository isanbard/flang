//===-- Type.h - Fortran Type Interface -------------------------*- C++ -*-===//
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

#ifndef FORTRAN_TYPE_H__
#define FORTRAN_TYPE_H__

#include "flang/Sema/Ownership.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
class raw_ostream;
} // end llvm namespace

namespace fortran {

class ASTContext;
class Decl;
class Expr;

//===----------------------------------------------------------------------===//
/// Selector - A selector is a modifier on a type that indicates different
/// properties for the type: precision, length, etc.
class Selector {
  ExprResult KindExpr;
public:
  const ExprResult getKindExpr() const { return KindExpr; }
  ExprResult getKindExpr() { return KindExpr; }
  void setKindExpr(ExprResult KE) { KindExpr = KE; }

  void print(llvm::raw_ostream &O) const;
};

//===----------------------------------------------------------------------===//
/// Type - The base class for Fortran types.
class Type {
protected:
  /// TypeClass - The intrinsic Fortran type specifications. REAL is the default
  /// if "IMPLICIT NONE" isn't specified.
  enum TypeClass {
    None    = 0,
    Builtin = 1,
    Complex = 2,
    Array   = 3,
    Struct  = 4,
    Pointer = 5
  };

private:
  TypeClass TyClass;
protected:
  Type(TypeClass tc) : TyClass(tc) {}
  virtual ~Type();
  virtual void Destroy(ASTContext &C);
  friend class ASTContext;
public:
  TypeClass getTypeClass() const { return TyClass; }

  virtual void print(llvm::raw_ostream &O) const = 0;

  static bool classof(const Type *) { return true; }
};

//===----------------------------------------------------------------------===//
/// BuiltinType - Intrinsic Fortran types.
class BuiltinType : public Type, public llvm::FoldingSetNode {
public:
  /// TypeSpec - The intrinsic Fortran type specifications. REAL is the default
  /// if "IMPLICIT NONE" isn't specified.
  enum TypeSpec {
    TS_Invalid         = -1,
    TS_Integer         = 0,
    TS_Real            = 1,
    TS_DoublePrecision = 2,
    TS_Complex         = 3,
    TS_Character       = 4,
    TS_Logical         = 5
  };
protected:
  TypeSpec TySpec;              //< Type specification.
  Selector Kind;                //< Kind selector.

  friend class ASTContext;      // ASTContext creates these.
  BuiltinType() : Type(Builtin), TySpec(TS_Real) {}
  BuiltinType(TypeSpec TS) : Type(Builtin), TySpec(TS) {}
  BuiltinType(TypeSpec TS, Selector K)
    : Type(Builtin), TySpec(TS), Kind(K)
  {}
public:
  virtual ~BuiltinType();

  TypeSpec getTypeSpec() const { return TySpec; }

  bool hasKind() const { return Kind.getKindExpr().isUsable(); }
  Selector getKind() const { return Kind; }

  bool isIntegerType() const { return TySpec == TS_Integer; }
  bool isRealType() const { return TySpec == TS_Real; }
  bool isDoublePrecisionType() const { return TySpec == TS_DoublePrecision; }
  bool isCharacterType() const { return TySpec == TS_Character; }
  bool isLogicalType() const { return TySpec == TS_Logical; }

  virtual void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, TySpec, Kind);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, TypeSpec TS, Selector K) {
    ID.AddInteger(TS);
    ID.AddPointer(K.getKindExpr().get());
  }

  virtual void print(llvm::raw_ostream &O) const;

  static bool classof(const Type *T) { return T->getTypeClass() == Builtin; }
  static bool classof(const BuiltinType *) { return true; }
};

//===----------------------------------------------------------------------===//
/// CharacterBuiltinType - A character builtin type has an optional 'LEN' kind
/// selector.
class CharacterBuiltinType : public BuiltinType {
  Selector Len;             //< Optional length selector.
  friend class ASTContext;  // ASTContext creates these.
  CharacterBuiltinType(Selector L, Selector K)
    : BuiltinType(TS_Character, K), Len(L) {}
public:
  virtual ~CharacterBuiltinType();

  bool hasLen() const { return Len.getKindExpr().isUsable(); }
  Selector getLen() const { return Len; }
  void setLen(Selector L) { Len = L; }

  virtual void Profile(llvm::FoldingSetNodeID &ID) {
    BuiltinType::Profile(ID);
    Profile(ID, Len, Kind);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, Selector L, Selector K) {
    ID.AddInteger(TS_Character);
    ID.AddPointer(L.getKindExpr().get());
    ID.AddPointer(K.getKindExpr().get());
  }

  virtual void print(llvm::raw_ostream &O) const;

  static bool classof(const Type *T) {
    return T->getTypeClass() == Builtin &&
      ((const BuiltinType*)T)->isCharacterType();
  }
  static bool classof(const BuiltinType *BT) { return BT->isCharacterType(); }
  static bool classof(const CharacterBuiltinType *) { return true; }
};

//===----------------------------------------------------------------------===//
/// PointerType - Allocatable types.
class PointerType : public Type, public llvm::FoldingSetNode {
  const Type *BaseType;     //< The type of the object pointed to.
  unsigned NumDims;
  friend class ASTContext;  // ASTContext creates these.
  PointerType(const Type *BaseTy, unsigned Dims)
    : Type(Pointer), BaseType(BaseTy), NumDims(Dims) {}
public:
  const Type *getPointeeType() const { return BaseType; }
  unsigned getNumDimensions() const { return NumDims; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getPointeeType(), getNumDimensions());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, const Type *ElemTy,
                      unsigned NumDims) {
    ID.AddPointer(ElemTy);
    ID.AddInteger(NumDims);
  }

  virtual void print(llvm::raw_ostream &O) const {} // FIXME

  static bool classof(const Type *T) { return T->getTypeClass() == Pointer; }
  static bool classof(const PointerType *) { return true; }
};

//===----------------------------------------------------------------------===//
/// ArrayType - Array types.
class ArrayType : public Type, public llvm::FoldingSetNode {
  const Type *ElemType;
  llvm::SmallVector<unsigned, 4> Dimensions;
  friend class ASTContext;  // ASTContext creates these.
  ArrayType(const Type *ElemTy, llvm::ArrayRef<unsigned> Dims)
    : Type(Array), ElemType(ElemTy) {
    Dimensions.append(Dims.begin(), Dims.end());
  }
public:
  const Type *getElementType() const { return ElemType; }
  const llvm::SmallVectorImpl<unsigned> &getDimensions() const {
    return Dimensions;
  }

  typedef llvm::SmallVectorImpl<unsigned>::iterator dim_iterator;
  typedef llvm::SmallVectorImpl<unsigned>::const_iterator const_dim_iterator;

  size_t size() const              { return Dimensions.size(); }
  dim_iterator begin()             { return Dimensions.begin(); }
  dim_iterator end()               { return Dimensions.end(); }
  const_dim_iterator begin() const { return Dimensions.begin(); }
  const_dim_iterator end() const   { return Dimensions.end(); }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, ElemType, Dimensions);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, const Type *ElemTy,
                      const llvm::SmallVectorImpl<unsigned> &Dims) {
    ID.AddPointer(ElemTy);

    for (llvm::SmallVectorImpl<unsigned>::const_iterator
           I = Dims.begin(), E = Dims.end(); I != E; ++I)
      ID.AddInteger(*I);
  }

  virtual void print(llvm::raw_ostream &O) const {} // FIXME

  static bool classof(const Type *T) { return T->getTypeClass() == Array; }
  static bool classof(const ArrayType *) { return true; }
};

//===----------------------------------------------------------------------===//
/// StructType - Structure types.
class StructType : public Type, public llvm::FoldingSetNode {
  std::vector<Decl*> Elems;
  friend class ASTContext;  // ASTContext creates these.
  StructType(llvm::ArrayRef<Decl*> Elements)
    : Type(Struct), Elems(Elements.begin(), Elements.end()) {}
public:
  Decl *getElement(unsigned Idx) const { return Elems[Idx]; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Elems);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, llvm::ArrayRef<Decl*> Elems) {
    for (llvm::ArrayRef<Decl*>::iterator
           I = Elems.begin(), E = Elems.end(); I != E; ++I)
      ID.AddPointer(*I);
  }

  virtual void print(llvm::raw_ostream &O) const {} // FIXME

  static bool classof(const Type *T) { return T->getTypeClass() == Struct; }
  static bool classof(const StructType *) { return true; }
};

} // end fortran namespace

#endif
