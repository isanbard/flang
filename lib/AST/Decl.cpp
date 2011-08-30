//===--- Decl.cpp - Classes for representing Declarations ------------------==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Decl and related classes.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/Decl.h"
#include "flang/AST/DeclContextInternals.h"
#include "flang/AST/Expr.h"
#include "flang/AST/ASTContext.h"
using namespace fortran;

Decl::~Decl() {}

DeclContext *Decl::castToDeclContext(const Decl *D) {
  Decl::Kind DK = D->getKind();
  switch(DK) {
#define DECL(NAME, BASE)
#define DECL_CONTEXT(NAME) \
    case Decl::NAME:       \
      return static_cast<NAME##Decl*>(const_cast<Decl*>(D));
#define DECL_CONTEXT_BASE(NAME)
#include "flang/AST/DeclNodes.inc"
    default:
#define DECL(NAME, BASE)
#define DECL_CONTEXT_BASE(NAME)                                   \
      if (DK >= first##NAME && DK <= last##NAME)                  \
        return static_cast<NAME##Decl*>(const_cast<Decl*>(D));
#include "flang/AST/DeclNodes.inc"
      assert(false && "a decl that inherits DeclContext isn't handled");
      return 0;
  }
}

DeclContext::~DeclContext() {}

DeclContext *DeclContext::getPrimaryContext() {
  // FIXME: Could be more than just 'this'.
  return this;
}

DeclContext::decl_iterator DeclContext::decls_begin() const {
  return decl_iterator(FirstDecl);
}

DeclContext::decl_iterator DeclContext::decls_end() const {
  return decl_iterator();
}

bool DeclContext::decls_empty() const {
  return !FirstDecl;
}

void DeclContext::addDecl(Decl *D) {
  assert(D->getDeclContext() == this &&
         "Decl inserted into wrong lexical context");
  assert(!D->getNextDeclInContext() && D != LastDecl &&
         "Decl already inserted into a DeclContext");

  if (FirstDecl) {
    LastDecl->NextDeclInContext = D;
    LastDecl = D;
  } else {
    FirstDecl = LastDecl = D;
  }

  if (NamedDecl *ND = dyn_cast<NamedDecl>(D))
    ND->getDeclContext()->makeDeclVisibleInContext(ND);
}

void DeclContext::removeDecl(Decl *D) {
  assert((D->NextDeclInContext || D == LastDecl) &&
         "decl is not in decls list");

  // Remove D from the decl chain.  This is O(n) but hopefully rare.
  if (D == FirstDecl) {
    if (D == LastDecl)
      FirstDecl = LastDecl = 0;
    else
      FirstDecl = D->NextDeclInContext;
  } else {
    for (Decl *I = FirstDecl; true; I = I->NextDeclInContext) {
      assert(I && "Decl not found in linked list");
      if (I->NextDeclInContext == D) {
        I->NextDeclInContext = D->NextDeclInContext;
        if (D == LastDecl) LastDecl = I;
        break;
      }
    }
  }

  // Mark that D is no longer in the decl chain.
  D->NextDeclInContext = 0;

  // Remove D from the lookup table if necessary.
  if (NamedDecl *ND = dyn_cast<NamedDecl>(D)) {
    // Remove only decls that have a name
    if (!ND->getDeclName()) return;

    StoredDeclsMap *Map = getPrimaryContext()->LookupPtr;
    if (!Map) return;

    StoredDeclsMap::iterator Pos = Map->find(ND->getDeclName());
    assert(Pos != Map->end() && "No lookup entry for decl");
    Pos->second.remove(ND);
  }
}

/// buildLookup - Build the lookup data structure with all of the declarations
/// in DCtx (and any other contexts linked to it or transparent contexts nested
/// within it).
void DeclContext::buildLookup(DeclContext *DCtx) {
  for (decl_iterator
         D = DCtx->decls_begin(), DEnd = DCtx->decls_end(); D != DEnd; ++D)
    // Insert this declaration into the lookup structure, but only if it's
    // semantically in its decl context.  During non-lazy lookup building, this
    // is implicitly enforced by addDecl.
    if (NamedDecl *ND = dyn_cast<NamedDecl>(*D))
      if (D->getDeclContext() == DCtx)
        makeDeclVisibleInContextImpl(ND);
}

DeclContext::lookup_result
DeclContext::lookup(DeclarationName Name) {
  DeclContext *PrimaryContext = getPrimaryContext();
  if (PrimaryContext != this)
    return PrimaryContext->lookup(Name);

  /// If there is no lookup data structure, build one now by walking
  /// all of the linked DeclContexts (in declaration order!) and
  /// inserting their values.
  if (!LookupPtr) {
    buildLookup(this);

    if (!LookupPtr)
      return lookup_result(lookup_iterator(0), lookup_iterator(0));
  }

  StoredDeclsMap::iterator Pos = LookupPtr->find(Name);
  if (Pos == LookupPtr->end())
    return lookup_result(lookup_iterator(0), lookup_iterator(0));
  return Pos->second.getLookupResult();
}

DeclContext::lookup_const_result
DeclContext::lookup(DeclarationName Name) const {
  return const_cast<DeclContext*>(this)->lookup(Name);
}

void DeclContext::makeDeclVisibleInContext(NamedDecl *D) {
#if 0
  DeclContext *PrimaryContext = getPrimaryContext();
  if (PrimaryContext != this) {
    PrimaryContext->makeDeclVisibleInContext(D);
    return;
  }

  // If we already have a lookup data structure, perform the insertion
  // into it. If we haven't deserialized externally stored decls, deserialize
  // them so we can add the decl. Otherwise, be lazy and don't build that
  // structure until someone asks for it.
  if (LookupPtr || hasExternalVisibleStorage())
    makeDeclVisibleInContextImpl(D);

  // If we are a transparent context or inline namespace, insert into our
  // parent context, too. This operation is recursive.
  if (isTransparentContext() || isInlineNamespace())
    getParent()->makeDeclVisibleInContext(D);

  Decl *DCAsDecl = cast<Decl>(this);
  // Notify that a decl was made visible unless it's a Tag being defined. 
  if (!(isa<TagDecl>(DCAsDecl) && cast<TagDecl>(DCAsDecl)->isBeingDefined()))
    if (ASTMutationListener *L = DCAsDecl->getASTMutationListener())
      L->AddedVisibleDecl(this, D);
#endif
}

void DeclContext::makeDeclVisibleInContextImpl(NamedDecl *D) {
#if 0
  // Skip unnamed declarations.
  if (!D->getDeclName())
    return;

  // Skip entities that can't be found by name lookup into a particular
  // context.
  if (D->getIdentifierNamespace() == 0)
    return;

  ASTContext *C = 0;
  if (!LookupPtr) {
    C = &getParentASTContext();
    CreateStoredDeclsMap(*C);
  }

  // If there is an external AST source, load any declarations it knows about
  // with this declaration's name.
  // If the lookup table contains an entry about this name it means that we
  // have already checked the external source.
  if (ExternalASTSource *Source = getParentASTContext().getExternalSource())
    if (hasExternalVisibleStorage() &&
        LookupPtr->find(D->getDeclName()) == LookupPtr->end())
      Source->FindExternalVisibleDeclsByName(this, D->getDeclName());

  // Insert this declaration into the map.
  StoredDeclsList &DeclNameEntries = (*LookupPtr)[D->getDeclName()];
  if (DeclNameEntries.isNull()) {
    DeclNameEntries.setOnlyValue(D);
    return;
  }

  // If it is possible that this is a redeclaration, check to see if there is
  // already a decl for which declarationReplaces returns true.  If there is
  // one, just replace it and return.
  if (DeclNameEntries.HandleRedeclaration(D))
    return;

  // Put this declaration into the appropriate slot.
  DeclNameEntries.AddSubsequentDecl(D);
#endif
}

//===----------------------------------------------------------------------===//
// RecordDecl Implementation
//===----------------------------------------------------------------------===//

RecordDecl *RecordDecl::Create(const ASTContext &C, DeclContext *DC,
                               llvm::SMLoc StartLoc, llvm::SMLoc IdLoc,
                               IdentifierInfo *Id, RecordDecl *PrevDecl) {
  RecordDecl* R = new (C) RecordDecl(Record, DC, StartLoc, IdLoc, Id, PrevDecl);
  C.getTypeDeclType(R, PrevDecl);
  return R;
}

//===----------------------------------------------------------------------===//
// Other Decl Allocation/Deallocation Method Implementations
//===----------------------------------------------------------------------===//

TranslationUnitDecl *TranslationUnitDecl::Create(ASTContext &C) {
  return new (C) TranslationUnitDecl(C);
}

EnumConstantDecl *EnumConstantDecl::Create(ASTContext &C, DeclContext *DC,
                                           llvm::SMLoc L, IdentifierInfo *Id,
                                           QualType T, Expr *E,
                                           const llvm::APSInt &V) {
  return new (C) EnumConstantDecl(DC, L, Id, T, E, V);
}

SourceRange EnumConstantDecl::getSourceRange() const {
  llvm::SMLoc End = getLocation();
  if (Init)
    End = Init->getLocation();  // FIXME: getLocEnd() ?
  return SourceRange(getLocation(), End);
}

//===----------------------------------------------------------------------===//
// FieldDecl Implementation
//===----------------------------------------------------------------------===//

FieldDecl *FieldDecl::Create(const ASTContext &C, DeclContext *DC,
                             llvm::SMLoc StartLoc, llvm::SMLoc IdLoc,
                             IdentifierInfo *Id, QualType T) {
  return new (C) FieldDecl(Decl::Field, DC, StartLoc, IdLoc, Id, T);
}

//===----------------------------------------------------------------------===//
// VarDecl Implementation
//===----------------------------------------------------------------------===//

VarDecl *VarDecl::Create(ASTContext &C, DeclContext *DC,
                         llvm::SMLoc StartL, llvm::SMLoc IdL,
                         IdentifierInfo *Id, QualType T) {
  return new (C) VarDecl(Var, DC, StartL, IdL, Id, T);
}

#if 0
//===----------------------------------------------------------------------===//
// Creation and Destruction of StoredDeclsMaps.                               //
//===----------------------------------------------------------------------===//

StoredDeclsMap *DeclContext::CreateStoredDeclsMap(ASTContext &C) const {
  assert(!LookupPtr && "Context already has a decls map");
  assert(getPrimaryContext() == this &&
         "Creating decls map on non-primary context");

  StoredDeclsMap *M = new StoredDeclsMap();
  M->Previous = C.LastSDM;
  C.LastSDM = llvm::PointerIntPair<StoredDeclsMap*,1>(M, Dependent);
  LookupPtr = M;
  return M;
}

void ASTContext::ReleaseDeclContextMaps() {
  // It's okay to delete DependentStoredDeclsMaps via a StoredDeclsMap
  // pointer because the subclass doesn't add anything that needs to
  // be deleted.
  StoredDeclsMap::DestroyAll(LastSDM.getPointer(), LastSDM.getInt());
}

void StoredDeclsMap::DestroyAll(StoredDeclsMap *Map, bool Dependent) {
  while (Map) {
    // Advance the iteration before we invalidate memory.
    llvm::PointerIntPair<StoredDeclsMap*,1> Next = Map->Previous;

    if (Dependent)
      delete static_cast<DependentStoredDeclsMap*>(Map);
    else
      delete Map;

    Map = Next.getPointer();
    Dependent = Next.getInt();
  }
}
#endif
