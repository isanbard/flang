//===--- Ownership.h - Parser ownership helpers -----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file contains classes for managing ownership of Stmt and Expr nodes.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_SEMA_OWNERSHIP_H__
#define FLANG_SEMA_OWNERSHIP_H__

#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"

namespace flang {

class Decl;
class Expr;
class QualType;
class Stmt;

/// OpaquePtr - This is a very simple POD type that wraps a pointer that the
/// Parser doesn't know about but that Sema or another client does. The UID
/// template argument is used to make sure that "Decl" pointers are not
/// compatible with "Type" pointers for example.
template <class PtrTy>
class OpaquePtr {
  void *Ptr;
  explicit OpaquePtr(void *Ptr) : Ptr(Ptr) {}
  typedef llvm::PointerLikeTypeTraits<PtrTy> Traits;
public:
  OpaquePtr() : Ptr(0) {}

  static OpaquePtr make(PtrTy P) { OpaquePtr OP; OP.set(P); return OP; }

  template <typename T> T* getAs() const {
    return get();
  }

  template <typename T> T getAsVal() const {
    return get();
  }

  PtrTy get() const {
    return Traits::getFromVoidPointer(Ptr);
  }

  void set(PtrTy P) {
    Ptr = Traits::getAsVoidPointer(P);
  }

  operator bool() const { return Ptr != 0; }

  void *getAsOpaquePtr() const { return Ptr; }
  static OpaquePtr getFromOpaquePtr(void *P) { return OpaquePtr(P); }
};

} // end flang namespace

namespace llvm {
  template <class T>
  class PointerLikeTypeTraits<flang::OpaquePtr<T> > {
  public:
    static inline void *getAsVoidPointer(flang::OpaquePtr<T> P) {
      // FIXME: Doesn't work? return P.getAs< void >();
      return P.getAsOpaquePtr();
    }
    static inline flang::OpaquePtr<T> getFromVoidPointer(void *P) {
      return flang::OpaquePtr<T>::getFromOpaquePtr(P);
    }
    enum { NumLowBitsAvailable = 0 };
  };

  template <class T>
  struct isPodLike<flang::OpaquePtr<T> > { static const bool value = true; };
}

namespace flang {

/// ActionResult - This structure is used while parsing and acting on
/// expressions, stmts, etc.
template <typename PtrTy>
class ActionResult {
  PtrTy Val;
  bool Invalid;
public:
  ActionResult(bool invalid = false) : Val(PtrTy()), Invalid(invalid) {}
  ActionResult(PtrTy val) : Val(val), Invalid(false) {}

  // These two overloads prevent void* -> bool conversions.
  ActionResult(const void *);
  ActionResult(volatile void *);

  bool isInvalid() const { return Invalid; }
  bool isUsable() const { return !Invalid && Val; }

  PtrTy get() const { return Val; }
  PtrTy release() const { return Val; }
  PtrTy take() const { return Val; }
  template <typename T> T *takeAs() { return static_cast<T*>(get()); }

  void set(PtrTy V) { Val = V; }

  const ActionResult &operator=(PtrTy RHS) {
    Val = RHS;
    Invalid = false;
    return *this;
  }
};

/// An opaque type for threading parsed type information through the parser.
typedef OpaquePtr<QualType> ParsedType;
typedef ActionResult<ParsedType> TypeResult;

typedef ActionResult<Decl*> DeclResult;
typedef ActionResult<Expr*> ExprResult;
typedef ActionResult<Stmt*> StmtResult;

} // end flang namespace

#endif
