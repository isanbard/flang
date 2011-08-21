//===-- Decl.h - Declarations -----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Classes for Fortran declarations.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_AST_DECL_H__
#define FORTRAN_AST_DECL_H__

#include "flang/Basic/IdentifierTable.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

namespace fortran {

class ASTContext;
class DeclSpec;
class IdentifierInfo;

//===----------------------------------------------------------------------===//
/// VarDecl - Declaration or definition of a variable.
///
class VarDecl : public llvm::FoldingSetNode {
  llvm::SMLoc Loc;
  const DeclSpec *DS;
  const IdentifierInfo *IDInfo;

  /// HasAttrs - This indicates whether the decl has attributes or not.
  bool HasAttrs : 1;

  friend class ASTContext;  // ASTContext creates these.
public:
  VarDecl(const IdentifierInfo *Info)
    : DS(0), IDInfo(Info), HasAttrs(false)
  {}
  VarDecl(llvm::SMLoc L, const DeclSpec *dts, const IdentifierInfo *Info)
    : Loc(L), DS(dts), IDInfo(Info), HasAttrs(false)
  {}

  llvm::SMLoc getLocation() const { return Loc; }
  void setLocation(llvm::SMLoc L) { Loc = L; }

  const IdentifierInfo *getIdentifier() const { return IDInfo; }
  void setIdentifier(const IdentifierInfo *II) { IDInfo = II; }

  const DeclSpec *getDeclSpec() const { return DS; }
  void setDeclSpec(const DeclSpec *Val) { DS = Val; }

  /// isImplicitlyDefined - A variable which isn't defined and there isn't an
  /// "implicit none" statement has a default type of REAL.
  bool isImplicitlyDefined() const { return DS == 0; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, IDInfo);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, const IdentifierInfo *IDInfo){
    ID.AddPointer(IDInfo);
  }

  // Implement isa/cast/dyn_cast/etc.
  static bool classof(const VarDecl *D) { return true; }
};

static inline llvm::raw_ostream &operator<<(llvm::raw_ostream &O,
                                            const VarDecl &V) {
  return O << V.getIdentifier()->getName();
}

} // end fortran namespace

#endif // FORTRAN_AST_DECL_H__
