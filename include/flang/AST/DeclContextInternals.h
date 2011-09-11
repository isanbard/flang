//===-- DeclContextInternals.h - DeclContext Representation -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the data structures used in the implementation
//  of DeclContext.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_AST_DECLCONTEXTINTERNALS_H__
#define FLANG_AST_DECLCONTEXTINTERNALS_H__

#include "flang/AST/Decl.h"
#include "flang/AST/DeclarationName.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include <algorithm>

namespace flang {

class DependentDiagnostic;

/// StoredDeclsList - This is an array of decls optimized for the common case of
/// only containing one entry.
struct StoredDeclsList {
  /// DeclsTy - When in vector form, this is what the Data pointer points to.
  typedef SmallVector<NamedDecl*, 4> DeclsTy;

  /// \brief The stored data, which will be either a pointer to a NamedDecl, or
  /// a pointer to a vector.
  llvm::PointerUnion<NamedDecl*, DeclsTy*> Data;
public:
  StoredDeclsList() {}
  StoredDeclsList(const StoredDeclsList &RHS) : Data(RHS.Data) {
    if (DeclsTy *RHSVec = RHS.getAsVector())
      Data = new DeclsTy(*RHSVec);
  }
  ~StoredDeclsList() {
    // If this is a vector-form, free the vector.
    if (DeclsTy *Vector = getAsVector())
      delete Vector;
  }

  StoredDeclsList &operator=(const StoredDeclsList &RHS) {
    if (DeclsTy *Vector = getAsVector())
      delete Vector;
    Data = RHS.Data;
    if (DeclsTy *RHSVec = RHS.getAsVector())
      Data = new DeclsTy(*RHSVec);
    return *this;
  }

  bool isNull() const { return Data.isNull(); }

  NamedDecl *getAsDecl() const {
    return Data.dyn_cast<NamedDecl *>();
  }
  DeclsTy *getAsVector() const {
    return Data.dyn_cast<DeclsTy *>();
  }

  void setOnlyValue(NamedDecl *ND) {
    assert(!getAsVector() && "Not inline");
    Data = ND;
    // Make sure that Data is a plain NamedDecl* so we can use its address at
    // getLookupResult.
    assert(*(NamedDecl **)&Data == ND &&
           "PointerUnion mangles the NamedDecl pointer!");
  }

  void remove(NamedDecl *D) {
    assert(!isNull() && "removing from empty list");
    if (NamedDecl *Singleton = getAsDecl()) {
      assert(Singleton == D && "list is different singleton");
      (void)Singleton;
      Data = (NamedDecl *)0;
      return;
    }

    DeclsTy &Vec = *getAsVector();
    DeclsTy::iterator I = std::find(Vec.begin(), Vec.end(), D);
    assert(I != Vec.end() && "list does not contain decl");
    Vec.erase(I);

    assert(std::find(Vec.begin(), Vec.end(), D)
             == Vec.end() && "list still contains decl");
  }

  /// getLookupResult - Return an array of all the decls that this list
  /// represents.
  DeclContext::lookup_result getLookupResult() {
    if (isNull())
      return DeclContext::lookup_result(DeclContext::lookup_iterator(0),
                                        DeclContext::lookup_iterator(0));

    // If we have a single NamedDecl, return it.
    if (getAsDecl()) {
      assert(!isNull() && "Empty list isn't allowed");

      // Data is a raw pointer to a NamedDecl*, return it.
      void *Ptr = &Data;
      return DeclContext::lookup_result((NamedDecl**)Ptr, (NamedDecl**)Ptr+1);
    }

    assert(getAsVector() && "Must have a vector at this point");
    DeclsTy &Vector = *getAsVector();

    // Otherwise, we have a range result.
    return DeclContext::lookup_result(&Vector[0], &Vector[0]+Vector.size());
  }

  /// AddSubsequentDecl - This is called on the second and later decl when it is
  /// not a redeclaration to merge it into the appropriate place in our list.
  void AddSubsequentDecl(NamedDecl *D) {
    // If this is the second decl added to the list, convert this to vector
    // form.
    if (NamedDecl *OldD = getAsDecl()) {
      DeclsTy *VT = new DeclsTy();
      VT->push_back(OldD);
      Data = VT;
    }

    getAsVector()->push_back(D);
  }
};

class StoredDeclsMap : public llvm::DenseMap<DeclarationName, StoredDeclsList> {
  friend class ASTContext; // Walks the chain deleting these.
  friend class DeclContext;
  StoredDeclsMap *Previous;
public:
  static void DestroyAll(StoredDeclsMap *Map);
};

} // end namespace flang

#endif
