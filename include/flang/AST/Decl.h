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
#include "llvm/Support/PrettyStackTrace.h"

namespace llvm {
  class SourceMgr;
}

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

  /// InvalidDecl - This indicates a semantic error occurred.
  unsigned InvalidDecl :  1;

  /// HasAttrs - This indicates whether the decl has attributes or not.
  unsigned HasAttrs : 1;

  /// Implicit - Whether this declaration was implicitly generated by
  /// the implementation rather than explicitly written by the user.
  unsigned Implicit : 1;

protected:

  Decl(Kind DK, DeclContext *DC, llvm::SMLoc L)
    : NextDeclInContext(0), DeclCtx(DC), Loc(L), DeclKind(DK),
      InvalidDecl(false), HasAttrs(false), Implicit(false) {}

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

#if 0
  bool hasAttrs() const { return HasAttrs; }
  void setAttrs(const AttrVec& Attrs);
  AttrVec &getAttrs() {
    return const_cast<AttrVec&>(const_cast<const Decl*>(this)->getAttrs());
  }
  const AttrVec &getAttrs() const;
  void swapAttrs(Decl *D);
  void dropAttrs();

  void addAttr(Attr *A) {
    if (hasAttrs())
      getAttrs().push_back(A);
    else
      setAttrs(AttrVec(1, A));
  }
#endif

  /// setInvalidDecl - Indicates the Decl had a semantic error. This
  /// allows for graceful error recovery.
  void setInvalidDecl(bool Invalid = true);
  bool isInvalidDecl() const { return (bool) InvalidDecl; }

  /// isImplicit - Indicates whether the declaration was implicitly
  /// generated by the implementation. If false, this declaration
  /// was written explicitly in the source code.
  bool isImplicit() const { return Implicit; }
  void setImplicit(bool I = true) { Implicit = I; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *) { return true; }
};

/// PrettyStackTraceDecl - If a crash occurs, indicate that it happened when
/// doing something to a specific decl.
class PrettyStackTraceDecl : public llvm::PrettyStackTraceEntry {
  const Decl *TheDecl;
  llvm::SMLoc Loc;
  llvm::SourceMgr &SM;
  const char *Message;
public:
  PrettyStackTraceDecl(const Decl *theDecl, llvm::SMLoc L,
                       llvm::SourceMgr &sm, const char *Msg)
    : TheDecl(theDecl), Loc(L), SM(sm), Message(Msg) {}

  virtual void print(raw_ostream &OS) const;
};

/// DeclContext - This is used only as base class of specific decl types that
/// can act as declaration contexts. These decls are (only the top classes
/// that directly derive from DeclContext are mentioned, not their subclasses):
///
///   TranslationUnitDecl
///   FunctionDecl
///   TagDecl
///
class DeclContext {
  /// DeclKind - This indicates which class this is.
  unsigned DeclKind : 8;
protected:
  /// FirstDecl - The first declaration stored within this declaration
  /// context.
  mutable Decl *FirstDecl;

  /// LastDecl - The last declaration stored within this declaration
  /// context.
  mutable Decl *LastDecl;

  DeclContext(Decl::Kind K)
    : DeclKind(K), FirstDecl(0), LastDecl(0) {}
public:
  ~DeclContext();

  Decl::Kind getDeclKind() const {
    return static_cast<Decl::Kind>(DeclKind);
  }

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
  const DeclSpec *DS;

  friend class ASTContext;  // ASTContext creates these.
public:
  VarDecl(const IdentifierInfo *Info)
    // FIXME:
    : ValueDecl(Var, 0, llvm::SMLoc(), DeclarationName(Info), 0),
      DS(0)
  {}
  VarDecl(llvm::SMLoc L, const DeclSpec *dts, const IdentifierInfo *Info)
    // FIXME:
    : ValueDecl(Var, 0, L, DeclarationName(Info), 0),
      DS(dts)
  {}

  const DeclSpec *getDeclSpec() const { return DS; }
  void setDeclSpec(const DeclSpec *Val) { DS = Val; }

  /// isImplicitlyDefined - A variable which isn't defined and there isn't an
  /// "implicit none" statement has a default type of REAL.
  bool isImplicitlyDefined() const { return DS == 0; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getIdentifier());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, const IdentifierInfo *Info) {
    ID.AddPointer(Info);
  }

  // Implement isa/cast/dyn_cast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= FirstVar && D->getKind() <= LastVar;
  }
  static bool classof(const VarDecl *) { return true; }
};

static inline llvm::raw_ostream &operator<<(llvm::raw_ostream &O,
                                            const VarDecl &V) {
  return O << V.getIdentifier()->getName();
}

/// TypeDecl - Represents a declaration of a type.
class TypeDecl : public NamedDecl {
  /// TypeForDecl - This indicates the Type object that represents this
  /// TypeDecl. It is a cache maintained by ASTContext::getTagDeclType.
  mutable const class Type *TypeForDecl;

  /// LocStart - The start of the source range for this declaration.
  llvm::SMLoc LocStart;

  friend class ASTContext;
  friend class DeclContext;
  friend class TagDecl;
  friend class TagType;

protected:
  TypeDecl(Kind DK, DeclContext *DC, llvm::SMLoc L, IdentifierInfo *Id,
           llvm::SMLoc StartL = llvm::SMLoc())
    : NamedDecl(DK, DC, L, Id), TypeForDecl(0), LocStart(StartL) {}

public:
  // Low-level accessor
  const class Type *getTypeForDecl() const { return TypeForDecl; }
  void setTypeForDecl(const class Type *TD) { TypeForDecl = TD; }

  llvm::SMLoc getLocStart() const { return LocStart; }
  void setLocStart(llvm::SMLoc L) { LocStart = L; }
  virtual SourceRange getSourceRange() const {
    if (LocStart.isValid())
      return SourceRange(LocStart, getLocation());
    else
      return SourceRange(getLocation());
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= FirstType && D->getKind() <= LastType;
  }
  static bool classof(const TypeDecl *D) { return true; }
};

} // end fortran namespace

#endif // FORTRAN_AST_DECL_H__
