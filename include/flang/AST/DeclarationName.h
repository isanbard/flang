//===-- DeclarationName.h - Representation of declaration names -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the DeclarationName and DeclarationNameTable classes.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_AST_DECLARATIONNAME_H__
#define FLANG_AST_DECLARATIONNAME_H__

#include "flang/Basic/IdentifierTable.h"
#include "flang/Basic/SourceLocation.h"
#include "flang/AST/Type.h"

namespace llvm {
  template <typename T> struct DenseMapInfo;
}

namespace flang {

class DeclarationNameTable;
class IdentifierInfo;
class MultiKeywordSelector;
class NamedDecl;
class TypeSourceInfo;

/// DeclarationName - The name of a declaration. In the common case, this just
/// stores an IdentifierInfo pointer to a normal name
class DeclarationName {
public:
  /// NameKind - The kind of name this object contains.
  enum NameKind {
    Identifier
  };

private:
  /// StoredNameKind - The kind of name that is actually stored in the upper
  /// bits of the Ptr field. This is only used internally.
  enum StoredNameKind {
    StoredIdentifier = 0,
    PtrMask = 0x03
  };

  /// Ptr - The lowest two bits are used to express what kind of name we're
  /// actually storing, using the values of NameKind. Depending on the kind of
  /// name this is, the upper bits of Ptr may have one of several different
  /// meanings:
  ///
  ///   StoredIdentifier - The name is a normal identifier, and Ptr is a normal
  ///   IdentifierInfo pointer.
  ///
  uintptr_t Ptr;

  /// getStoredNameKind - Return the kind of object that is stored in Ptr.
  StoredNameKind getStoredNameKind() const {
    return static_cast<StoredNameKind>(Ptr & PtrMask);
  }

  /// Construct a declaration name from a raw pointer.
  DeclarationName(uintptr_t Ptr) : Ptr(Ptr) {}

  friend class DeclarationNameTable;
  friend class NamedDecl;

  /// getFETokenInfoAsVoid - Retrieves the front end-specified pointer
  /// for this name as a void pointer.
  void *getFETokenInfoAsVoid() const;

public:
  /// DeclarationName - Used to create an empty selector.
  DeclarationName() : Ptr(0) {}

  // Construct a declaration name from an IdentifierInfo*.
  DeclarationName(const IdentifierInfo *II)
    : Ptr(reinterpret_cast<uintptr_t>(II)) {
    assert((Ptr & PtrMask) == 0 && "Improperly aligned IdentifierInfo");
  }

  // operator bool() - Evaluates true when this declaration name is non-empty.
  operator bool() const {
    return ((Ptr & PtrMask) != 0) ||
           (reinterpret_cast<IdentifierInfo *>(Ptr & ~PtrMask));
  }

  /// Predicate functions for querying what type of name this is.
  bool isIdentifier() const { return getStoredNameKind() == StoredIdentifier; }

  /// getNameKind - Determine what kind of name this is.
  NameKind getNameKind() const;

  /// getAsString - Retrieve the human-readable string for this name.
  std::string getAsString() const;

  /// printName - Print the human-readable name to a stream.
  void printName(llvm::raw_ostream &OS) const;

  /// getAsIdentifierInfo - Retrieve the IdentifierInfo* stored in this
  /// declaration name, or NULL if this declaration name isn't a simple
  /// identifier.
  IdentifierInfo *getAsIdentifierInfo() const {
    if (isIdentifier())
      return reinterpret_cast<IdentifierInfo *>(Ptr);
    return 0;
  }

  /// getAsOpaqueInteger - Get the representation of this declaration name as an
  /// opaque integer.
  uintptr_t getAsOpaqueInteger() const { return Ptr; }

  /// getAsOpaquePtr - Get the representation of this declaration name as an
  /// opaque pointer.
  void *getAsOpaquePtr() const { return reinterpret_cast<void*>(Ptr); }

  static DeclarationName getFromOpaquePtr(void *P) {
    DeclarationName N;
    N.Ptr = reinterpret_cast<uintptr_t> (P);
    return N;
  }
  static DeclarationName getFromOpaqueInteger(uintptr_t P) {
    DeclarationName N;
    N.Ptr = P;
    return N;
  }

  /// getFETokenInfo/setFETokenInfo - The language front-end is allowed to
  /// associate arbitrary metadata with some kinds of declaration names,
  /// including normal identifiers and conversion functions.
  template<typename T>
  T *getFETokenInfo() const { return static_cast<T*>(getFETokenInfoAsVoid()); }

  void setFETokenInfo(void *T);

  /// operator== - Determine whether the specified names are identical.
  friend bool operator==(DeclarationName LHS, DeclarationName RHS) {
    return LHS.Ptr == RHS.Ptr;
  }

  /// operator!= - Determine whether the specified names are different.
  friend bool operator!=(DeclarationName LHS, DeclarationName RHS) {
    return LHS.Ptr != RHS.Ptr;
  }

  static DeclarationName getEmptyMarker() {
    return DeclarationName(uintptr_t(-1));
  }

  static DeclarationName getTombstoneMarker() {
    return DeclarationName(uintptr_t(-2));
  }

  static int compare(DeclarationName LHS, DeclarationName RHS);
  
  void dump() const;
};

/// Ordering on two declaration names. If both names are identifiers, this
/// provides a lexicographical ordering.
inline bool operator<(DeclarationName LHS, DeclarationName RHS) {
  return DeclarationName::compare(LHS, RHS) < 0;
}

/// Ordering on two declaration names. If both names are identifiers, this
/// provides a lexicographical ordering.
inline bool operator>(DeclarationName LHS, DeclarationName RHS) {
  return DeclarationName::compare(LHS, RHS) > 0;
}

/// Ordering on two declaration names. If both names are identifiers, this
/// provides a lexicographical ordering.
inline bool operator<=(DeclarationName LHS, DeclarationName RHS) {
  return DeclarationName::compare(LHS, RHS) <= 0;
}

/// Ordering on two declaration names. If both names are identifiers, this
/// provides a lexicographical ordering.
inline bool operator>=(DeclarationName LHS, DeclarationName RHS) {
  return DeclarationName::compare(LHS, RHS) >= 0;
}

/// DeclarationNameTable - Used to store and retrieve DeclarationName instances
/// for the various kinds of declaration names, e.g., normal identifiers, etc.
class DeclarationNameTable {
  const ASTContext &Ctx;

  DeclarationNameTable(const DeclarationNameTable&);            // NONCOPYABLE
  DeclarationNameTable& operator=(const DeclarationNameTable&); // NONCOPYABLE

public:
  DeclarationNameTable(const ASTContext &C) : Ctx(C) {}
  ~DeclarationNameTable();

  /// getIdentifier - Create a declaration name that is a simple
  /// identifier.
  DeclarationName getIdentifier(const IdentifierInfo *ID) {
    return DeclarationName(ID);
  }
};

/// DeclarationNameInfo - A collector data type for bundling together a
/// DeclarationName and the correspnding source/type location info.
class DeclarationNameInfo {
  /// Name - The declaration name, also encoding name kind.
  DeclarationName Name;
  /// Loc - The main source location for the declaration name.
  llvm::SMLoc NameLoc;

public:
  DeclarationNameInfo(DeclarationName Name, llvm::SMLoc NameLoc)
    : Name(Name), NameLoc(NameLoc) {}

  /// getName - Returns the embedded declaration name.
  DeclarationName getName() const { return Name; }
  /// setName - Sets the embedded declaration name.
  void setName(DeclarationName N) { Name = N; }

  /// getLoc - Returns the main location of the declaration name.
  llvm::SMLoc getLoc() const { return NameLoc; }
  /// setLoc - Sets the main location of the declaration name.
  void setLoc(llvm::SMLoc L) { NameLoc = L; }

  /// getAsString - Retrieve the human-readable string for this name.
  std::string getAsString() const;

  /// printName - Print the human-readable name to a stream.
  void printName(llvm::raw_ostream &OS) const;

  /// getBeginLoc - Retrieve the location of the first token.
  llvm::SMLoc getBeginLoc() const { return NameLoc; }
  /// getEndLoc - Retrieve the location of the last token.
  llvm::SMLoc getEndLoc() const;
  /// getSourceRange - The range of the declaration name.
  SourceRange getSourceRange() const {
    return SourceRange(getBeginLoc(), getEndLoc());
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     DeclarationNameInfo DNInfo) {
  DNInfo.printName(OS);
  return OS;
}

}  // end namespace flang

namespace llvm {

/// Define DenseMapInfo so that DeclarationNames can be used as keys in DenseMap
/// and DenseSets.
template<>
struct DenseMapInfo<flang::DeclarationName> {
  static inline flang::DeclarationName getEmptyKey() {
    return flang::DeclarationName::getEmptyMarker();
  }

  static inline flang::DeclarationName getTombstoneKey() {
    return flang::DeclarationName::getTombstoneMarker();
  }

  static unsigned getHashValue(flang::DeclarationName);

  static inline bool
  isEqual(flang::DeclarationName LHS, flang::DeclarationName RHS) {
    return LHS == RHS;
  }
};

template <>
struct isPodLike<flang::DeclarationName> { static const bool value = true; };

}  // end namespace llvm

#endif
