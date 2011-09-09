//===-- DeclarationName.cpp - Declaration names implementation ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the DeclarationName and DeclarationNameTable classes.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/ASTContext.h"
#include "flang/AST/Decl.h"
#include "flang/AST/DeclarationName.h"
#include "flang/Basic/IdentifierTable.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace flang;

//===----------------------------------------------------------------------===//
// DeclarationName Implementation
//===----------------------------------------------------------------------===//

DeclarationName::NameKind DeclarationName::getNameKind() const {
  switch (getStoredNameKind()) {
  default: break;
  case StoredIdentifier:
    return Identifier;
  }

  // Can't actually get here.
  assert(false && "This should be unreachable!");
  return Identifier;
}

std::string DeclarationName::getAsString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  printName(OS);
  return OS.str();
}

void DeclarationName::printName(llvm::raw_ostream &OS) const {
  if (getNameKind() == Identifier) {
    if (const IdentifierInfo *II = getAsIdentifierInfo())
      OS << II->getName();
    return;
  }

  assert(false && "Unexpected declaration name kind");
}

void *DeclarationName::getFETokenInfoAsVoid() const {
  switch (getNameKind()) {
  case Identifier:
    return getAsIdentifierInfo()->getFETokenInfo<void>();
  }

  assert(false && "Declaration name has no FETokenInfo");
  return 0;
}

void DeclarationName::setFETokenInfo(void *T) {
  switch (getNameKind()) {
  case Identifier:
    getAsIdentifierInfo()->setFETokenInfo(T);
    break;
  }

  assert(false && "Declaration name has no FETokenInfo");
}

int DeclarationName::compare(DeclarationName LHS, DeclarationName RHS) {
  if (LHS.getNameKind() != RHS.getNameKind())
    return (LHS.getNameKind() < RHS.getNameKind() ? -1 : 1);

  switch (LHS.getNameKind()) {
  case DeclarationName::Identifier: {
    IdentifierInfo *LII = LHS.getAsIdentifierInfo();
    IdentifierInfo *RII = RHS.getAsIdentifierInfo();
    if (!LII) return RII ? -1 : 0;
    if (!RII) return 1;

    return LII->getName().compare(RII->getName());
  }
  }

  return 0;
}

void DeclarationName::dump() const {
  printName(llvm::errs());
  llvm::errs() << '\n';
}

//===----------------------------------------------------------------------===//
// DeclarationNameTable Implementation
//===----------------------------------------------------------------------===//

DeclarationNameTable::~DeclarationNameTable() {}

//===----------------------------------------------------------------------===//
// DeclarationNameInfo Implementation
//===----------------------------------------------------------------------===//

std::string DeclarationNameInfo::getAsString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  printName(OS);
  return OS.str();
}

void DeclarationNameInfo::printName(llvm::raw_ostream &OS) const {
  switch (Name.getNameKind()) {
  case DeclarationName::Identifier:
    Name.printName(OS);
    return;
  }

  assert(false && "Unexpected declaration name kind");
}

llvm::SMLoc DeclarationNameInfo::getEndLoc() const {
  switch (Name.getNameKind()) {
  case DeclarationName::Identifier:
    return NameLoc;
  }

  assert(false && "Unexpected declaration name kind");
  return llvm::SMLoc();
}

//===----------------------------------------------------------------------===//
// DenseMapInfo Implementation
//===----------------------------------------------------------------------===//

unsigned
llvm::DenseMapInfo<flang::DeclarationName>::
getHashValue(flang::DeclarationName N) {
  return DenseMapInfo<void*>::getHashValue(N.getAsOpaquePtr());
}
