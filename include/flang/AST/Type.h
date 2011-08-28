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
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
  class raw_ostream;
} // end llvm namespace

namespace fortran {
  enum {
    TypeAlignmentInBits = 4,
    TypeAlignment = 1 << TypeAlignmentInBits
  };
  class Type;
  class ExtQuals;
  class QualType;
}

namespace llvm {
  template <typename T>
  class PointerLikeTypeTraits;
  template<>
  class PointerLikeTypeTraits< ::fortran::Type*> {
  public:
    static inline void *getAsVoidPointer(::fortran::Type *P) { return P; }
    static inline ::fortran::Type *getFromVoidPointer(void *P) {
      return static_cast< ::fortran::Type*>(P);
    }
    enum { NumLowBitsAvailable = fortran::TypeAlignmentInBits };
  };
  template<>
  class PointerLikeTypeTraits< ::fortran::ExtQuals*> {
  public:
    static inline void *getAsVoidPointer(::fortran::ExtQuals *P) { return P; }
    static inline ::fortran::ExtQuals *getFromVoidPointer(void *P) {
      return static_cast< ::fortran::ExtQuals*>(P);
    }
    enum { NumLowBitsAvailable = fortran::TypeAlignmentInBits };
  };

  template <>
  struct isPodLike<fortran::QualType> { static const bool value = true; };
}

namespace fortran {

class ASTContext;
class Decl;
class Expr;
class ExtQualsTypeCommonBase;
class QualifierCollector;

/// Qualifiers - The collection of all-type qualifiers we support.
class Qualifiers {
public:
  enum TQ {
    Allocatable     = 1 << 0,
    Asynchronous    = 1 << 1,
    Contiguous      = 1 << 2,
    Optional        = 1 << 3,
    Parameter       = 1 << 4,
    Save            = 1 << 5,
    Target          = 1 << 6,
    Value           = 1 << 7,
    Volatile        = 1 << 8,
    QualMask        = 0x1FF
  };

  enum IntentAttr {
    IA_None = 0,
    In      = 1 << 0,
    Out     = 1 << 1,
    InOut   = In | Out
  };

  enum {
    /// The maximum supported address space number.
    /// 21 bits should be enough for anyone.
    MaxAddressSpace = 0x1FFFFFU
  };
private:
  // bits: |0   ..   8|9...10|11   ...  31|
  //       |Attributes|Intent|AddressSpace|
  uint32_t Mask;

  static const uint32_t IntentAttrShift = 9;
  static const uint32_t IntentAttrMask = 0x3 << IntentAttrShift;
  static const uint32_t AddressSpaceMask = ~(QualMask | InOut);
  static const uint32_t AddressSpaceShift = 11;
public:
  Qualifiers() : Mask(0) {}

  // Deserialize qualifiers from an opaque representation.
  static Qualifiers fromOpaqueValue(unsigned opaque) {
    Qualifiers Qs;
    Qs.Mask = opaque;
    return Qs;
  }

  // Serialize these qualifiers into an opaque representation.
  unsigned getAsOpaqueValue() const {
    return Mask;
  }

  bool hasAllocatable() const { return Mask & Allocatable; }
  void setAllocatable(bool flag) {
    Mask = (Mask & ~Allocatable) | (flag ? Allocatable : 0);
  } 
  void removeAllocatable() { Mask &= ~Allocatable; }
  void addAllocatable() { Mask |= Allocatable; }

  bool hasAsynchronous() const { return Mask & Asynchronous; }
  void setAsynchronous(bool flag) {
    Mask = (Mask & ~Asynchronous) | (flag ? Asynchronous : 0);
  } 
  void removeAsynchronous() { Mask &= ~Asynchronous; }
  void addAsynchronous() { Mask |= Asynchronous; }

  bool hasContiguous() const { return Mask & Contiguous; }
  void setContiguous(bool flag) {
    Mask = (Mask & ~Contiguous) | (flag ? Contiguous : 0);
  } 
  void removeContiguous() { Mask &= ~Contiguous; }
  void addContiguous() { Mask |= Contiguous; }

  bool hasOptional() const { return Mask & Optional; }
  void setOptional(bool flag) {
    Mask = (Mask & ~Optional) | (flag ? Optional : 0);
  } 
  void removeOptional() { Mask &= ~Optional; }
  void addOptional() { Mask |= Optional; }

  bool hasParameter() const { return Mask & Parameter; }
  void setParameter(bool flag) {
    Mask = (Mask & ~Parameter) | (flag ? Parameter : 0);
  } 
  void removeParameter() { Mask &= ~Parameter; }
  void addParameter() { Mask |= Parameter; }

  bool hasSave() const { return Mask & Save; }
  void setSave(bool flag) {
    Mask = (Mask & ~Save) | (flag ? Save : 0);
  } 
  void removeSave() { Mask &= ~Save; }
  void addSave() { Mask |= Save; }

  bool hasTarget() const { return Mask & Target; }
  void setTarget(bool flag) {
    Mask = (Mask & ~Target) | (flag ? Target : 0);
  } 
  void removeTarget() { Mask &= ~Target; }
  void addTarget() { Mask |= Target; }

  bool hasValue() const { return Mask & Value; }
  void setValue(bool flag) {
    Mask = (Mask & ~Value) | (flag ? Value : 0);
  } 
  void removeValue() { Mask &= ~Value; }
  void addValue() { Mask |= Value; }

  bool hasVolatile() const { return Mask & Volatile; }
  void setVolatile(bool flag) {
    Mask = (Mask & ~Volatile) | (flag ? Volatile : 0);
  } 
  void removeVolatile() { Mask &= ~Volatile; }
  void addVolatile() { Mask |= Volatile; }

  bool hasIntentAttr() const { return Mask & IntentAttrMask; }
  IntentAttr getIntentAttr() const {
    return IntentAttr((Mask & IntentAttrMask) >> IntentAttrShift);
  }
  void setIntentAttr(IntentAttr type) {
    Mask = (Mask & ~IntentAttrMask) | (type << IntentAttrShift);
  }
  void removeIntentAttr() { setIntentAttr(IA_None); }
  void addIntentAttr(IntentAttr type) {
    assert(type);
    setIntentAttr(type);
  }

  bool hasAddressSpace() const { return Mask & AddressSpaceMask; }
  unsigned getAddressSpace() const { return Mask >> AddressSpaceShift; }
  void setAddressSpace(unsigned space) {
    assert(space <= MaxAddressSpace);
    Mask = (Mask & ~AddressSpaceMask)
         | (((uint32_t) space) << AddressSpaceShift);
  }
  void removeAddressSpace() { setAddressSpace(0); }
  void addAddressSpace(unsigned space) {
    assert(space);
    setAddressSpace(space);
  }

  /// hasQualifiers - Return true if the set contains any qualifiers.
  bool hasQualifiers() const { return Mask; }
  bool empty() const { return !Mask; }

  /// \brief Add the qualifiers from the given set to this set.
  void addQualifiers(Qualifiers Q) {
    // If the other set doesn't have any non-boolean qualifiers, just
    // bit-or it in.
    if (!(Q.Mask & ~QualMask)) {
      Mask |= Q.Mask;
    } else {
      Mask |= (Q.Mask & QualMask);
      if (Q.hasAddressSpace())
        addAddressSpace(Q.getAddressSpace());
      if (Q.hasIntentAttr())
        addIntentAttr(Q.getIntentAttr());
    }
  }

  /// \brief Add the qualifiers from the given set to this set, given that
  /// they don't conflict.
  void addConsistentQualifiers(Qualifiers qs) {
    assert(getAddressSpace() == qs.getAddressSpace() ||
           !hasAddressSpace() || !qs.hasAddressSpace());
    assert(getIntentAttr() == qs.getIntentAttr() ||
           !hasIntentAttr() || !qs.hasIntentAttr());
    Mask |= qs.Mask;
  }

  bool operator==(Qualifiers Other) const { return Mask == Other.Mask; }
  bool operator!=(Qualifiers Other) const { return Mask != Other.Mask; }

  operator bool() const { return hasQualifiers(); }

  Qualifiers &operator+=(Qualifiers R) {
    addQualifiers(R);
    return *this;
  }

  // Union two qualifier sets. If an enumerated qualifier appears in both sets,
  // use the one from the right.
  friend Qualifiers operator+(Qualifiers L, Qualifiers R) {
    L += R;
    return L;
  }

  Qualifiers &operator-=(Qualifiers R) {
    Mask = Mask & ~(R.Mask);
    return *this;
  }

  /// \brief Compute the difference between two qualifier sets.
  friend Qualifiers operator-(Qualifiers L, Qualifiers R) {
    L -= R;
    return L;
  }

#if 0
  std::string getAsString() const;
  std::string getAsString(const PrintingPolicy &Policy) const {
    std::string Buffer;
    getAsStringInternal(Buffer, Policy);
    return Buffer;
  }
  void getAsStringInternal(std::string &S, const PrintingPolicy &Policy) const;
#endif
  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddInteger(Mask);
  }
};

/// QualType - 
class QualType {
  typedef llvm::PointerUnion<const Type*, const ExtQuals*> TypeQualUnion;
  TypeQualUnion Value;

  const ExtQuals *getExtQualsUnsafe() const {
    return Value.get<const ExtQuals*>();
  }

  const Type *getTypePtrUnsafe() const {
    return Value.get<const Type*>();
  }

  const ExtQualsTypeCommonBase *getCommonPtr() const {
    assert(!isNull() && "Cannot retrieve a NULL type pointer");
    uintptr_t CommonPtrVal
      = reinterpret_cast<uintptr_t>(Value.getOpaqueValue());
    CommonPtrVal &= ~(uintptr_t)((1 << TypeAlignmentInBits) - 1);
    return reinterpret_cast<ExtQualsTypeCommonBase*>(CommonPtrVal);
  }
  friend class QualifierCollector;
public:
  QualType() {}

  explicit QualType(const Type *Ptr)
    : Value(Ptr) {}
  explicit QualType(const ExtQuals *Ptr)
    : Value(Ptr) {}

  /// isNull - Return true if this QualType doesn't point to a type yet.
  bool isNull() const {
    return Value.isNull();
  }

  /// \brief Retrieves a pointer to the underlying (unqualified) type. This
  /// function requires that the type not be NULL. If the type might be NULL,
  /// use the (slightly less efficient) \c getTypePtrOrNull().
  const Type *getTypePtr() const;
  const Type *getTypePtrOrNull() const;

  void *getAsOpaquePtr() const { return Value.getOpaqueValue(); }
  static QualType getFromOpaquePtr(const void *Ptr) {
    QualType T;
    T.Value = llvm::PointerLikeTypeTraits<TypeQualUnion>
      ::getFromVoidPointer(const_cast<void*>(Ptr));
    return T;
  }

  const Type &operator*() const {
    return *getTypePtr();
  }
  const Type *operator->() const {
    return getTypePtr();
  }

  /// operator==/!= - Indicate whether the specified types and qualifiers are
  /// identical.
  friend bool operator==(const QualType &LHS, const QualType &RHS) {
    return LHS.Value == RHS.Value;
  }
  friend bool operator!=(const QualType &LHS, const QualType &RHS) {
    return LHS.Value != RHS.Value;
  }

#if 0
  void dump(const char *s) const;
  void dump() const;
#endif

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddPointer(getAsOpaquePtr());
  }
};

} // end fortran namespace

namespace llvm {

// Teach SmallPtrSet that QualType is "basically a pointer".
template<>
class PointerLikeTypeTraits<fortran::QualType> {
public:
  static inline void *getAsVoidPointer(fortran::QualType P) {
    return P.getAsOpaquePtr();
  }
  static inline fortran::QualType getFromVoidPointer(void *P) {
    return fortran::QualType::getFromOpaquePtr(P);
  }
  // Various qualifiers go in low bits.
  enum { NumLowBitsAvailable = 0 };
};

} // end namespace llvm

namespace fortran {

/// \brief Base class that is common to both the \c ExtQuals and \c Type 
/// classes, which allows \c QualType to access the common fields between the
/// two.
///
class ExtQualsTypeCommonBase {
  ExtQualsTypeCommonBase(const Type *BaseTy, QualType Canon)
    : BaseType(BaseTy), CanonicalType(Canon) {}

  /// \brief The "base" type of an extended qualifiers type (\c ExtQuals) or
  /// a self-referential pointer (for \c Type).
  ///
  /// This pointer allows an efficient mapping from a QualType to its 
  /// underlying type pointer.
  const Type *const BaseType;

  /// \brief The canonical type of this type.  A QualType.
  QualType CanonicalType;

  friend class QualType;
  friend class Type;
  friend class ExtQuals;
};

/// Selector - A selector is a modifier on a type that indicates different
/// properties for the type: precision, length, etc.
class Selector {
  friend class ASTContext;      // ASTContext creates these.
  ExprResult KindExpr;
public:
  const ExprResult getKindExpr() const { return KindExpr; }
  ExprResult getKindExpr() { return KindExpr; }
  void setKindExpr(ExprResult KE) { KindExpr = KE; }

  void print(llvm::raw_ostream &O) const;
};

/// Type - This is the base class for the type hierarchy.
class Type {
protected:
  /// TypeClass - The intrinsic Fortran type specifications. REAL is the default
  /// if "IMPLICIT NONE" isn't specified.
  enum TypeClass {
    None    = 0,
    Builtin = 1,
    Complex = 2,
    Array   = 3,
    Record  = 4,
    Pointer = 5
  };

private:
  Type(const Type&);           // DO NOT IMPLEMENT.
  void operator=(const Type&); // DO NOT IMPLEMENT.

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

/// ArrayType - Array types.
class ArrayType : public Type, public llvm::FoldingSetNode {
  const Type *ElemType;
  llvm::SmallVector<Expr*, 4> Dimensions;
  friend class ASTContext;  // ASTContext creates these.
  ArrayType(const Type *ElemTy, llvm::ArrayRef<Expr*> Dims)
    : Type(Array), ElemType(ElemTy) {
    Dimensions.append(Dims.begin(), Dims.end());
  }
public:
  const Type *getElementType() const { return ElemType; }
  const llvm::SmallVectorImpl<Expr*> &getDimensions() const {
    return Dimensions;
  }

  typedef llvm::SmallVectorImpl<Expr*>::iterator dim_iterator;
  typedef llvm::SmallVectorImpl<Expr*>::const_iterator const_dim_iterator;

  size_t size() const              { return Dimensions.size(); }
  dim_iterator begin()             { return Dimensions.begin(); }
  dim_iterator end()               { return Dimensions.end(); }
  const_dim_iterator begin() const { return Dimensions.begin(); }
  const_dim_iterator end() const   { return Dimensions.end(); }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, ElemType, Dimensions);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, const Type *ElemTy,
                      const llvm::SmallVectorImpl<Expr*> &Dims) {
    ID.AddPointer(ElemTy);

    for (llvm::SmallVectorImpl<Expr*>::const_iterator
           I = Dims.begin(), E = Dims.end(); I != E; ++I)
      ID.AddPointer(*I);
  }

  virtual void print(llvm::raw_ostream &O) const {} // FIXME

  static bool classof(const Type *T) { return T->getTypeClass() == Array; }
  static bool classof(const ArrayType *) { return true; }
};

/// RecordType - Record types.
class RecordType : public Type, public llvm::FoldingSetNode {
  std::vector<Decl*> Elems;
  friend class ASTContext;  // ASTContext creates these.
  RecordType(llvm::ArrayRef<Decl*> Elements)
    : Type(Record), Elems(Elements.begin(), Elements.end()) {}
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

  static bool classof(const Type *T) { return T->getTypeClass() == Record; }
  static bool classof(const RecordType *) { return true; }
};

// Inline function definitions.

inline const Type *QualType::getTypePtr() const {
  return getCommonPtr()->BaseType;
}
inline const Type *QualType::getTypePtrOrNull() const {
  return (isNull() ? 0 : getCommonPtr()->BaseType);
}

} // end fortran namespace

#endif
