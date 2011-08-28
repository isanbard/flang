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

#include "flang/AST/DeclarationName.h"
#include "flang/AST/Type.h"
#include "flang/Basic/IdentifierTable.h"
#include "flang/Basic/SourceLocation.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

namespace fortran {

class ASTContext;
class DeclSpec;
class DeclContext;
class IdentifierInfo;

//===----------------------------------------------------------------------===//
/// Decl - Base class for declarations.
///
class Decl {
public:
  enum Kind {
    Named,
      Type,
        Tag,
          Record,
          Enum,
      Value,
        Declarator,
          Var,
            ParamVar,
          Field,
        EnumConstant,
    FirstNamed = Named,           LastNamed = EnumConstant,
    FirstType = Type,             LastType = Enum,
    FirstTag = Tag,               LastTag = Enum,
    FirstValue = Value,           LastValue = EnumConstant,
    FirstDeclarator = Declarator, LastDeclarator = Field,
    FirstVar = Var,               LastVar = ParamVar
  };
private:
  friend class DeclContext;

  /// NextDeclInContext - The next declaration within the same lexical
  /// DeclContext. These pointers form the linked list that is traversed via
  /// DeclContext's decls_begin()/decls_end().
  Decl *NextDeclInContext;

  /// DeclCtx - The declaration context.
  DeclContext *DeclCtx;

  /// Loc - The location of this decl.
  llvm::SMLoc Loc;

  /// DeclKind - The class of decl this is.
  unsigned DeclKind : 8;

protected:

  Decl(Kind DK, DeclContext *DC, llvm::SMLoc L)
    : NextDeclInContext(0), DeclCtx(DC), Loc(L), DeclKind(DK) {}

  virtual ~Decl();

public:
  /// \brief Source range that this declaration covers.
  virtual SourceRange getSourceRange() const {
    return SourceRange(getLocation(), getLocation());
  }
  llvm::SMLoc getLocStart() const { return getSourceRange().getBegin(); }
  llvm::SMLoc getLocEnd() const { return getSourceRange().getEnd(); }

  llvm::SMLoc getLocation() const { return Loc; }
  void setLocation(llvm::SMLoc L) { Loc = L; }

  Kind getKind() const { return static_cast<Kind>(DeclKind); }

  Decl *getNextDeclInContext() { return NextDeclInContext; }
  const Decl *getNextDeclInContext() const { return NextDeclInContext; }

  DeclContext *getDeclContext() { return DeclCtx; }
  const DeclContext *getDeclContext() const { return DeclCtx; }
  void setDeclContext(DeclContext *DC) { DeclCtx = DC; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *) { return true; }
};

/// NamedDecl - This represents a decl with a name.
class NamedDecl : public Decl {
  /// Name - The name of this declaration, which is typically a normal
  /// identifier.
  DeclarationName Name;
protected:
  NamedDecl(Kind DK, DeclContext *DC, llvm::SMLoc L, DeclarationName N)
    : Decl(DK, DC, L), Name(N) {}
public:
  /// getIdentifier - Get the identifier that names this declaration, if there
  /// is one.
  IdentifierInfo *getIdentifier() const { return Name.getAsIdentifierInfo(); }

  /// getName - Get the name of identifier for this declaration as a StringRef.
  /// This requires that the declaration have a name and that it be a simple
  /// identifier.
  llvm::StringRef getName() const {
    assert(Name.isIdentifier() && "Name is not a simple identifier");
    return getIdentifier() ? getIdentifier()->getName() : "";
  }

  /// getDeclName - Get the actual, stored name of the declaration.
  DeclarationName getDeclName() const { return Name; }

  /// \brief Set the name of this declaration.
  void setDeclName(DeclarationName N) { Name = N; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= FirstNamed && D->getKind() <= LastNamed;
  }
  static bool classof(const NamedDecl *) { return true; }
};

/// ValueDecl - Represent the declaration of a variable (in which case it is an
/// lvalue), a function (in which case it is a function designator), or an enum
/// constant.
class ValueDecl : public NamedDecl {
  //FIXME:  QualType DeclType;
  class Type *DeclType;
protected:
  ValueDecl(Kind DK, DeclContext *DC, llvm::SMLoc L,
            DeclarationName N, class Type *T)
    : NamedDecl(DK, DC, L, N), DeclType(T) {}
public:
  class Type *getType() const { return DeclType; }
  void setType(class Type *newType) { DeclType = newType; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= FirstValue && D->getKind() <= LastValue;
  }
  static bool classof(const ValueDecl *D) { return true; }
};

/// VarDecl - An instance of this class is created to represent a variable
/// declaration or definition.
#if 0
// FIXME:
class VarDecl : public DeclaratorDecl {
#endif
class VarDecl : public ValueDecl, public llvm::FoldingSetNode {
  llvm::SMLoc Loc;
  const DeclSpec *DS;
  const IdentifierInfo *IDInfo;

  friend class ASTContext;  // ASTContext creates these.
public:
  VarDecl(const IdentifierInfo *Info)
    // FIXME:
    : ValueDecl(Var, 0, llvm::SMLoc(), DeclarationName(Info), 0),
      DS(0), IDInfo(Info)
  {}
  VarDecl(llvm::SMLoc L, const DeclSpec *dts, const IdentifierInfo *Info)
    // FIXME:
    : ValueDecl(Var, 0, L, DeclarationName(Info), 0),
      Loc(L), DS(dts), IDInfo(Info)
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
