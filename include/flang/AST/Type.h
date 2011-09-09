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

#include "flang/Basic/Specifiers.h"
#include "flang/Sema/Ownership.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "flang/Basic/LLVM.h"

namespace llvm {
  class raw_ostream;
} // end llvm namespace

namespace flang {
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
  class PointerLikeTypeTraits< ::flang::Type*> {
  public:
    static inline void *getAsVoidPointer(::flang::Type *P) { return P; }
    static inline ::flang::Type *getFromVoidPointer(void *P) {
      return static_cast< ::flang::Type*>(P);
    }
    enum { NumLowBitsAvailable = flang::TypeAlignmentInBits };
  };
  template<>
  class PointerLikeTypeTraits< ::flang::ExtQuals*> {
  public:
    static inline void *getAsVoidPointer(::flang::ExtQuals *P) { return P; }
    static inline ::flang::ExtQuals *getFromVoidPointer(void *P) {
      return static_cast< ::flang::ExtQuals*>(P);
    }
    enum { NumLowBitsAvailable = flang::TypeAlignmentInBits };
  };

  template <>
  struct isPodLike<flang::QualType> { static const bool value = true; };
}

namespace flang {

class ASTContext;
class Decl;
class Expr;
class ExtQualsTypeCommonBase;
class IdentifierInfo;
class QualifierCollector;

/// Qualifiers - The collection of all-type qualifiers we support.
class Qualifiers {
public:
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

  enum TQ { // NOTE: These flags must be kept in sync with DeclSpec::TQ.
    Allocatable = 1 << 0,
    Parameter   = 1 << 1,
    Volatile    = 1 << 2,
    APVMask     = (Allocatable | Parameter | Volatile)
  };

  enum ExtAttr {
    EA_None         = 0,
    Asynchronous    = 1 << 0,
    Contiguous      = 1 << 1,
    Optional        = 1 << 2,
    Pointer         = 1 << 3,
    Save            = 1 << 4,
    Target          = 1 << 5,
    Value           = 1 << 6
  };

  enum IntentAttr {
    IA_None = 0,
    In      = 1 << 0,
    Out     = 1 << 1,
    InOut   = In | Out
  };

  enum {
    /// The maximum supported address space number.
    MaxAddressSpace = 0xFFFU,

    /// The width of the "fast" qualifier mask.
    FastWidth = 3,

    /// The fast qualifier mask.
    FastMask = (1 << FastWidth) - 1
  };
private:
  // bits: |0  ...  14|15..17|18..19|20   ..   31|
  //       |Attributes|Intent|Access|AddressSpace|
  uint32_t Mask;

  static const uint32_t ExtAttrShift = 0;
  static const uint32_t ExtAttrMask = 0x7FFF << ExtAttrShift;
  static const uint32_t IntentAttrShift = 15;
  static const uint32_t IntentAttrMask = 0x7 << IntentAttrShift;
  static const uint32_t AccessAttrShift = 18;
  static const uint32_t AccessAttrMask = 0x3 << AccessAttrShift;
  static const uint32_t AddressSpaceShift = 20;
  static const uint32_t AddressSpaceMask =
    ~(APVMask | ExtAttrMask | IntentAttrMask);
public:
  Qualifiers() : Mask(0) {}

  static Qualifiers fromFastMask(unsigned Mask) {
    Qualifiers Qs;
    Qs.addFastQualifiers(Mask);
    return Qs;
  }

  static Qualifiers fromAPVMask(unsigned APV) {
    Qualifiers Qs;
    Qs.addAPVQualifiers(APV);
    return Qs;
  }

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

  /// Allocatable, Parameter, and Volatile attributes.
  bool hasAPVQualifiers() const { return getAPVQualifiers(); }
  unsigned getAPVQualifiers() const { return Mask & APVMask; }
  void setAPVQualifiers(unsigned mask) {
    assert(!(mask & ~APVMask) && "bitmask contains non-APV bits");
    Mask = (Mask & ~APVMask) | mask;
  }
  void removeAPVQualifiers(unsigned mask) {
    assert(!(mask & ~APVMask) && "bitmask contains non-APV bits");
    Mask &= ~mask;
  }
  void removeAPVQualifiers() {
    removeAPVQualifiers(APVMask);
  }
  void addAPVQualifiers(unsigned mask) {
    assert(!(mask & ~APVMask) && "bitmask contains non-APV bits");
    Mask |= mask;
  }

  bool hasAllocatable() const { return Mask & Allocatable; }
  void setAllocatable(bool flag) {
    Mask = (Mask & ~Allocatable) | (flag ? Allocatable : 0);
  } 
  void removeAllocatable() { Mask &= ~Allocatable; }
  void addAllocatable() { Mask |= Allocatable; }

  bool hasParameter() const { return Mask & Parameter; }
  void setParameter(bool flag) {
    Mask = (Mask & ~Parameter) | (flag ? Parameter : 0);
  } 
  void removeParameter() { Mask &= ~Parameter; }
  void addParameter() { Mask |= Parameter; }

  bool hasVolatile() const { return Mask & Volatile; }
  void setVolatile(bool flag) {
    Mask = (Mask & ~Volatile) | (flag ? Volatile : 0);
  } 
  void removeVolatile() { Mask &= ~Volatile; }
  void addVolatile() { Mask |= Volatile; }

  /// Extra attributes.
  bool hasExtAttr() const { return Mask & ExtAttrMask; }
  ExtAttr getExtAttr() const {
    return ExtAttr((Mask & ExtAttrMask) >> ExtAttrShift);
  }
  void setExtAttr(ExtAttr type) {
    Mask = (Mask & ~ExtAttrMask) | (type << ExtAttrShift);
  }
  void removeExtAttr() { setExtAttr(EA_None); }
  void addExtAttr(ExtAttr type) {
    assert(type);
    setExtAttr(type);
  }

  /// Intent attributes.
  bool hasIntentAttr() const { return Mask & IntentAttrMask; }
  IS getIntentAttr() const {
    return IS((Mask & IntentAttrMask) >> IntentAttrShift);
  }
  void setIntentAttr(IS type) {
    Mask = (Mask & ~IntentAttrMask) | (type << IntentAttrShift);
  }
  void removeIntentAttr() { setIntentAttr(IS_unspecified); }
  void addIntentAttr(IS type) {
    assert(type);
    setIntentAttr(type);
  }

  /// Access attributes.
  bool hasAccessAttr() const { return Mask & AccessAttrMask; }
  AC getAccessAttr() const {
    return AC((Mask & AccessAttrMask) >> AccessAttrShift);
  }
  void setAccessAttr(AC type) {
    Mask = (Mask & ~AccessAttrMask) | (type << AccessAttrShift);
  }
  void removeAccessAttr() { setAccessAttr(AC_unspecified); }
  void addAccessAttr(AC type) {
    assert(type);
    setAccessAttr(type);
  }

  /// Address space.
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

  // Fast qualifiers are those that can be allocated directly on a QualType
  // object.
  bool hasFastQualifiers() const { return getFastQualifiers(); }
  unsigned getFastQualifiers() const { return Mask & FastMask; }
  void setFastQualifiers(unsigned mask) {
    assert(!(mask & ~FastMask) && "bitmask contains non-fast qualifier bits");
    Mask = (Mask & ~FastMask) | mask;
  }
  void removeFastQualifiers(unsigned mask) {
    assert(!(mask & ~FastMask) && "bitmask contains non-fast qualifier bits");
    Mask &= ~mask;
  }
  void removeFastQualifiers() {
    removeFastQualifiers(FastMask);
  }
  void addFastQualifiers(unsigned mask) {
    assert(!(mask & ~FastMask) && "bitmask contains non-fast qualifier bits");
    Mask |= mask;
  }

  /// hasNonFastQualifiers - Return true if the set contains any
  /// qualifiers which require an ExtQuals node to be allocated.
  bool hasNonFastQualifiers() const { return Mask & ~FastMask; }
  Qualifiers getNonFastQualifiers() const {
    Qualifiers Quals = *this;
    Quals.setFastQualifiers(0);
    return Quals;
  }

  /// hasQualifiers - Return true if the set contains any qualifiers.
  bool hasQualifiers() const { return Mask; }
  bool empty() const { return !Mask; }

  /// \brief Add the qualifiers from the given set to this set.
  void addQualifiers(Qualifiers Q) {
    // If the other set doesn't have any non-boolean qualifiers, just
    // bit-or it in.
    if (!(Q.Mask & ~APVMask)) {
      Mask |= Q.Mask;
    } else {
      Mask |= (Q.Mask & APVMask);
      if (Q.hasAddressSpace())
        addAddressSpace(Q.getAddressSpace());
      if (Q.hasExtAttr())
        addExtAttr(Q.getExtAttr());
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
    assert(getExtAttr() == qs.getExtAttr() ||
           !hasExtAttr() || !qs.hasExtAttr());
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

typedef std::pair<const Type*, Qualifiers> SplitQualType;

/// QualType - For efficiency, some of the more common attributes are stored as
/// part of the type.
class QualType {
  // Thankfully, these are efficiently composable.
  llvm::PointerIntPair<llvm::PointerUnion<const Type*, const ExtQuals*>,
                       Qualifiers::FastWidth> Value;

  const ExtQuals *getExtQualsUnsafe() const {
    return Value.getPointer().get<const ExtQuals*>();
  }

  const Type *getTypePtrUnsafe() const {
    return Value.getPointer().get<const Type*>();
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

  explicit QualType(const Type *Ptr, unsigned Quals)
    : Value(Ptr, Quals) {}
  explicit QualType(const ExtQuals *Ptr, unsigned Quals)
    : Value(Ptr, Quals) {}

  unsigned getLocalFastQualifiers() const { return Value.getInt(); }
  void setLocalFastQualifiers(unsigned Quals) { Value.setInt(Quals); }

  /// \brief Retrieves a pointer to the underlying (unqualified) type. This
  /// function requires that the type not be NULL. If the type might be NULL,
  /// use the (slightly less efficient) \c getTypePtrOrNull().
  const Type *getTypePtr() const;
  const Type *getTypePtrOrNull() const;

  /// \brief Retrieves a pointer to the name of the base type.
  const IdentifierInfo *getBaseTypeIdentifier() const;

  /// \brief Divides a QualType into its unqualified type and a set of local
  /// qualifiers.
  SplitQualType split() const;

  void *getAsOpaquePtr() const { return Value.getOpaqueValue(); }
  static QualType getFromOpaquePtr(const void *Ptr) {
    QualType T;
    T.Value.setFromOpaqueValue(const_cast<void*>(Ptr));
    return T;
  }

  void addFastQualifiers(unsigned TQs) {
    assert(!(TQs & ~Qualifiers::FastMask)
           && "Non-fast qualifier bits set in mask!");
    Value.setInt(Value.getInt() | TQs);
  }

  // Qualifiers already on this type.
  QualType withFastQualifiers(unsigned TQs) const {
    QualType T = *this;
    T.addFastQualifiers(TQs);
    return T;
  }

  QualType getCanonicalType() const;

  /// \brief Determine whether this particular QualType instance has any
  /// "non-fast" qualifiers, e.g., those that are stored in an ExtQualType
  /// instance.
  bool hasLocalNonFastQualifiers() const {
    return Value.getPointer().is<const ExtQuals*>();
  }

  bool isCanonical() const;

  /// isNull - Return true if this QualType doesn't point to a type yet.
  bool isNull() const {
    return Value.getPointer().isNull();
  }

  /// \brief Determine whether this particular QualType instance has any
  /// qualifiers, without looking through any typedefs that might add 
  /// qualifiers at a different level.
  bool hasLocalQualifiers() const {
    return getLocalFastQualifiers() || hasLocalNonFastQualifiers();
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

  void print(raw_ostream &OS) const;
  void dump() const;

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddPointer(getAsOpaquePtr());
  }
};

} // end flang namespace

namespace llvm {

/// Implement simplify_type for QualType, so that we can dyn_cast from QualType
/// to a specific Type class.
template<> struct simplify_type<const ::flang::QualType> {
  typedef const ::flang::Type *SimpleType;
  static SimpleType getSimplifiedValue(const ::flang::QualType &Val) {
    return Val.getTypePtr();
  }
};
template<> struct simplify_type< ::flang::QualType>
  : public simplify_type<const ::flang::QualType> {};

// Teach SmallPtrSet that QualType is "basically a pointer".
template<>
class PointerLikeTypeTraits<flang::QualType> {
public:
  static inline void *getAsVoidPointer(flang::QualType P) {
    return P.getAsOpaquePtr();
  }
  static inline flang::QualType getFromVoidPointer(void *P) {
    return flang::QualType::getFromOpaquePtr(P);
  }
  // Various qualifiers go in low bits.
  enum { NumLowBitsAvailable = 0 };
};

} // end namespace llvm

namespace flang {

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

/// ExtQuals - We can encode up to four bits in the low bits of a type pointer,
/// but there are many more type qualifiers that we want to be able to apply to
/// an arbitrary type.  Therefore we have this struct, intended to be
/// heap-allocated and used by QualType to store qualifiers.
class ExtQuals : public ExtQualsTypeCommonBase, public llvm::FoldingSetNode {
  /// Quals - the immutable set of qualifiers applied by this node; always
  /// contains extended qualifiers.
  Qualifiers Quals;

  /// KindSelector - The kind-selector for a type.
  Expr *KindSelector;

  /// LenSelector - The kind-selector for a type.
  Expr *LenSelector;

  ExtQuals *this_() { return this; }

public:
  ExtQuals(const Type *BaseTy, QualType Canon, Qualifiers Quals,
           Expr *KS = 0, Expr *LS = 0)
    : ExtQualsTypeCommonBase(BaseTy,
                             Canon.isNull() ? QualType(this_(), 0) : Canon),
      Quals(Quals), KindSelector(KS), LenSelector(LS)
  {}

  Qualifiers getQualifiers() const { return Quals; }

  bool hasExtAttr() const { return Quals.hasExtAttr(); }
  unsigned getExtAttr() const { return Quals.getExtAttr(); }

  bool hasIntentAttr() const { return Quals.hasIntentAttr(); }
  unsigned getIntentAttr() const { return Quals.getIntentAttr(); }

  bool hasAddressSpace() const { return Quals.hasAddressSpace(); }
  unsigned getAddressSpace() const { return Quals.getAddressSpace(); }

  const Type *getBaseType() const { return BaseType; }

  bool hasKindSelector() const { return KindSelector != 0; }
  Expr *getKindSelector() const { return KindSelector; }

  bool hasLengthSelector() const { return LenSelector != 0; }
  Expr *getLengthSelector() const { return LenSelector; }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    Profile(ID, getBaseType(), Quals, KindSelector, LenSelector);
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      const Type *BaseType, Qualifiers Quals,
                      Expr *KS, Expr *LS) {
    ID.AddPointer(BaseType);
    ID.AddPointer(KS);
    ID.AddPointer(LS);
    Quals.Profile(ID);
  }
};

/// Type - This is the base class for the type hierarchy.
///
/// Types are immutable once created.
class Type : public ExtQualsTypeCommonBase {
protected:
  /// TypeClass - The intrinsic Fortran type specifications. REAL is the default
  /// if "IMPLICIT NONE" isn't specified.
  enum TypeClass {
    None    = 0,
    Builtin = 1,
    Record  = 2,
    Pointer = 3,
    Array   = 4,
    ConstantArray = 5
  };

private:
  Type(const Type&);           // DO NOT IMPLEMENT.
  void operator=(const Type&); // DO NOT IMPLEMENT.

  /// Bitfields required by the Type class.
  class TypeBitfields {
    friend class Type;

    /// TypeClass bitfield - Enum that specifies what subclass this belongs to.
    unsigned TC : 8;
  };
  enum { NumTypeBits = 8 };
protected:
  // These classes allow subclasses to somewhat cleanly pack bitfields
  // into Type.

  class BuiltinTypeBitfields {
    friend class BuiltinType;

    unsigned : NumTypeBits;

    /// The kind (BuiltinType::Kind) of builtin type this is.
    unsigned Kind : 8;
  };

  union {
    TypeBitfields TypeBits;
    BuiltinTypeBitfields BuiltinTypeBits;
  };

  Type *this_() { return this; }
  Type(TypeClass TC, QualType Canon)
    : ExtQualsTypeCommonBase(this,
                             Canon.isNull() ? QualType(this_(), 0) : Canon) {
    TypeBits.TC = TC;
  }

  friend class ASTContext;
public:
  TypeClass getTypeClass() const { return TypeClass(TypeBits.TC); }

  /// \brief Determines if this type would be canonical if it had no further
  /// qualification.
  bool isCanonicalUnqualified() const {
    return CanonicalType == QualType(this, 0);
  }

  QualType getCanonicalTypeInternal() const {
    return CanonicalType;
  }

  /// Helper methods to distinguish type categories. All type predicates
  /// operate on the canonical type ignoring qualifiers.

  /// isBuiltinType - returns true if the type is a builtin type.
  bool isBuiltinType() const;
  bool isIntegerType() const;
  bool isRealType() const;
  bool isCharacterType() const;
  bool isDoublePrecisionType() const;
  bool isComplexType() const;
  bool isLogicalType() const;
  bool isArrayType() const;
  bool isConstantArrayType() const;

  static bool classof(const Type *) { return true; }
};

/// BuiltinType - Intrinsic Fortran types.
class BuiltinType : public Type {
public:
  /// TypeSpec - The intrinsic Fortran type specifications. REAL is the default
  /// if "IMPLICIT NONE" isn't specified.
  enum TypeSpec {
    Invalid         = -1,
    Integer         = 0,
    Real            = 1,
    DoublePrecision = 2,
    Complex         = 3,
    Character       = 4,
    Logical         = 5
  };
protected:
  Expr *Kind;

  friend class ASTContext;      // ASTContext creates these.
  BuiltinType()
    : Type(Builtin, QualType()) {
    BuiltinTypeBits.Kind = Real;
  }
  BuiltinType(TypeSpec TS)
    : Type(Builtin, QualType()) {
    BuiltinTypeBits.Kind = TS;
  }
public:
  TypeSpec getTypeSpec() const { return TypeSpec(BuiltinTypeBits.Kind); }

  void print(llvm::raw_ostream &O) const;

  static bool classof(const Type *T) { return T->getTypeClass() == Builtin; }
  static bool classof(const BuiltinType *) { return true; }
};

/// CharacterBuiltinType - A character builtin type has an optional 'LEN' kind
/// selector.
class CharacterBuiltinType : public BuiltinType {
  Expr *Len;
  friend class ASTContext;  // ASTContext creates these.
  CharacterBuiltinType(Expr *L, Expr *K)
    : BuiltinType(Character), Len(L) {}
public:
  bool hasLen() const { return Len != 0; }
  Expr *getLen() const { return Len; }
  void setLen(Expr *L) { Len = L; }

  void print(llvm::raw_ostream &O) const;

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
    : Type(Pointer, QualType()), BaseType(BaseTy), NumDims(Dims) {}
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

  void print(llvm::raw_ostream &O) const {} // FIXME

  static bool classof(const Type *T) { return T->getTypeClass() == Pointer; }
  static bool classof(const PointerType *) { return true; }
};

/// ArrayType - Array types.
class ArrayType : public Type, public llvm::FoldingSetNode {
  QualType ElementType;
protected:
  ArrayType(TypeClass tc, QualType et, QualType can)
    : Type(tc, can), ElementType(et) {}

  friend class ASTContext;  // ASTContext creates these.
public:
  QualType getElementType() const { return ElementType; }

  void print(llvm::raw_ostream &O) const {} // FIXME

  static bool classof(const Type *T) {
    return T->getTypeClass() == ConstantArray;
  }
  static bool classof(const ArrayType *) { return true; }
};

/// ConstantArrayType - This class represents the canonical version of
/// Fortran arrays with a specified constant size.
class ConstantArrayType : public ArrayType {
  llvm::APInt Size;

  ConstantArrayType(QualType et, QualType canon, const llvm::APInt &size)
    : ArrayType(ConstantArray, et, canon), Size(size) {}
protected:
  friend class ASTContext;
public:
  const llvm::APInt &getSize() const { return Size; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getElementType(), getSize());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, QualType ET,
                      const llvm::APInt &ArraySize) {
    ID.AddPointer(ET.getAsOpaquePtr());
    ID.AddInteger(ArraySize.getZExtValue());
  }
  static bool classof(const Type *T) {
    return T->getTypeClass() == ConstantArray;
  }
  static bool classof(const ConstantArrayType *) { return true; }
};

/// RecordType - Record types.
class RecordType : public Type, public llvm::FoldingSetNode {
  std::vector<Decl*> Elems;
  friend class ASTContext;  // ASTContext creates these.
  RecordType(llvm::ArrayRef<Decl*> Elements)
    : Type(Record, QualType()), Elems(Elements.begin(), Elements.end()) {}
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

  void print(llvm::raw_ostream &O) const {} // FIXME

  static bool classof(const Type *T) { return T->getTypeClass() == Record; }
  static bool classof(const RecordType *) { return true; }
};

/// A qualifier set is used to build a set of qualifiers.
class QualifierCollector : public Qualifiers {
public:
  QualifierCollector(Qualifiers Qs = Qualifiers()) : Qualifiers(Qs) {}

  /// Collect any qualifiers on the given type and return an
  /// unqualified type.  The qualifiers are assumed to be consistent
  /// with those already in the type.
  const Type *strip(QualType type) {
    addFastQualifiers(type.getLocalFastQualifiers());
    if (!type.hasLocalNonFastQualifiers())
      return type.getTypePtrUnsafe();
      
    const ExtQuals *extQuals = type.getExtQualsUnsafe();
    addConsistentQualifiers(extQuals->getQualifiers());
    return extQuals->getBaseType();
  }

  /// Apply the collected qualifiers to the given type.
  QualType apply(const ASTContext &Context, QualType QT) const;

  /// Apply the collected qualifiers to the given type.
  QualType apply(const ASTContext &Context, const Type* T) const;
};

// Inline function definitions.

inline const Type *QualType::getTypePtr() const {
  return getCommonPtr()->BaseType;
}
inline const Type *QualType::getTypePtrOrNull() const {
  return (isNull() ? 0 : getCommonPtr()->BaseType);
}
inline bool QualType::isCanonical() const {
  return getTypePtr()->isCanonicalUnqualified();
}
inline SplitQualType QualType::split() const {
  if (!hasLocalNonFastQualifiers())
    return SplitQualType(getTypePtrUnsafe(),
                         Qualifiers::fromFastMask(getLocalFastQualifiers()));

  const ExtQuals *EQ = getExtQualsUnsafe();
  Qualifiers Qs = EQ->getQualifiers();
  Qs.addFastQualifiers(getLocalFastQualifiers());
  return SplitQualType(EQ->getBaseType(), Qs);
}
inline QualType QualType::getCanonicalType() const {
  QualType canon = getCommonPtr()->CanonicalType;
  return canon.withFastQualifiers(getLocalFastQualifiers());
}


inline bool Type::isBuiltinType() const {
  return isa<BuiltinType>(CanonicalType);
}
inline bool Type::isIntegerType() const {
  if (const BuiltinType *BT = dyn_cast<BuiltinType>(CanonicalType))
    return BT->getTypeSpec() == BuiltinType::Integer;
  return false;
}
inline bool Type::isRealType() const {
  if (const BuiltinType *BT = dyn_cast<BuiltinType>(CanonicalType))
    return BT->getTypeSpec() == BuiltinType::Real;
  return false;
}
inline bool Type::isCharacterType() const {
  if (const BuiltinType *BT = dyn_cast<BuiltinType>(CanonicalType))
    return BT->getTypeSpec() == BuiltinType::Character;
  return false;
}
inline bool Type::isDoublePrecisionType() const {
  if (const BuiltinType *BT = dyn_cast<BuiltinType>(CanonicalType))
    return BT->getTypeSpec() == BuiltinType::DoublePrecision;
  return false;
}
inline bool Type::isLogicalType() const {
  if (const BuiltinType *BT = dyn_cast<BuiltinType>(CanonicalType))
    return BT->getTypeSpec() == BuiltinType::Logical;
  return false;
}
inline bool Type::isComplexType() const {
  if (const BuiltinType *BT = dyn_cast<BuiltinType>(CanonicalType))
    return BT->getTypeSpec() == BuiltinType::Complex;
  return false;
}
inline bool Type::isArrayType() const {
  return isa<ArrayType>(CanonicalType);
}
inline bool Type::isConstantArrayType() const {
  return isa<ConstantArrayType>(CanonicalType);
}

} // end flang namespace

#endif
