//===--- Scope.h - Scope interface ------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Scope interface.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_SEMA_SCOPE_H__
#define FORTRAN_SEMA_SCOPE_H__

#include "flang/Basic/Diagnostic.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace fortran {

class Decl;
class UsingDirectiveDecl;

/// Scope - A scope is a transient data structure that is used while parsing the
/// program.  It assists with resolving identifiers to the appropriate
/// declaration.
///
class Scope {
public:
  /// ScopeFlags - These are bitfields that are or'd together when creating a
  /// scope, which defines the sorts of things the scope contains.
  enum ScopeFlags {
    /// FnScope - This indicates that the scope corresponds to a function, which
    /// means that labels are set here.
    FnScope     = 0x01,

    /// DeclScope - This is a scope that can contain a declaration. Some scopes
    /// just contain loop constructs but don't contain decls.
    DeclScope   = 0x02,

    /// RecordScope - The scope of a record definition.
    RecordScope = 0x04
  };
private:
  /// The parent scope for this scope.  This is null for the translation-unit
  /// scope.
  Scope *AnyParent;

  /// Depth - This is the depth of this scope.  The translation-unit scope has
  /// depth 0.
  unsigned short Depth;

  /// Flags - This contains a set of ScopeFlags, which indicates how the scope
  /// interrelates with other control flow statements.
  unsigned short Flags;

  /// PrototypeDepth - This is the number of function prototype scopes
  /// enclosing this scope, including this scope.
  unsigned short PrototypeDepth;

  /// PrototypeIndex - This is the number of parameters currently
  /// declared in this scope.
  unsigned short PrototypeIndex;

  /// FnParent - If this scope has a parent scope that is a function body, this
  /// pointer is non-null and points to it. This is used for label processing.
  Scope *FnParent;

  /// DeclsInScope - This keeps track of all declarations in this scope.  When
  /// the declaration is added to the scope, it is set as the current
  /// declaration for the identifier in the IdentifierTable.  When the scope is
  /// popped, these declarations are removed from the IdentifierTable's notion
  /// of current declaration.  It is up to the current Action implementation to
  /// implement these semantics.
  typedef llvm::SmallPtrSet<Decl*, 32> DeclSetTy;
  DeclSetTy DeclsInScope;

  /// Entity - The entity with which this scope is associated. For example, the
  /// entity of a record scope is the record itself, the entity of a function
  /// scope is a function, etc. This field is maintained by the Action
  /// implementation.
  void *Entity;

  /// \brief Used to determine if errors occurred in this scope.
  DiagnosticErrorTrap ErrorTrap;
  
public:
  Scope(Scope *Parent, unsigned ScopeFlags, Diagnostic &Diag)
    : ErrorTrap(Diag) {
    Init(Parent, ScopeFlags);
  }

  /// getFlags - Return the flags for this scope.
  ///
  unsigned getFlags() const { return Flags; }
  void setFlags(unsigned F) { Flags = F; }

  /// getParent - Return the scope that this is nested in.
  ///
  const Scope *getParent() const { return AnyParent; }
  Scope *getParent() { return AnyParent; }

  /// getFnParent - Return the closest scope that is a function body.
  ///
  const Scope *getFnParent() const { return FnParent; }
  Scope *getFnParent() { return FnParent; }

  /// Returns the number of function prototype scopes in this scope
  /// chain.
  unsigned getFunctionPrototypeDepth() const {
    return PrototypeDepth;
  }

  typedef DeclSetTy::iterator decl_iterator;
  decl_iterator decl_begin() const { return DeclsInScope.begin(); }
  decl_iterator decl_end()   const { return DeclsInScope.end(); }
  bool decl_empty()          const { return DeclsInScope.empty(); }

  void AddDecl(Decl *D) {
    DeclsInScope.insert(D);
  }

  void RemoveDecl(Decl *D) {
    DeclsInScope.erase(D);
  }

  /// isDeclScope - Return true if this is the scope that the specified decl is
  /// declared in.
  bool isDeclScope(Decl *D) {
    return DeclsInScope.count(D) != 0;
  }

  void* getEntity() const { return Entity; }
  void setEntity(void *E) { Entity = E; }

  bool hasErrorOccurred() const { return ErrorTrap.hasErrorOccurred(); }
                           
  /// isRecordScope - Return true if this scope is a structure scope.
  bool isRecordScope() const {
    return (getFlags() & Scope::RecordScope);
  }

  /// Init - This is used by the parser to implement scope caching.
  ///
  void Init(Scope *parent, unsigned flags);
};

}  // end namespace fortran

#endif
