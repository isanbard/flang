//===- Scope.cpp - Lexical scope information ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Scope class, which is used for recording information
// about a lexical scope.
//
//===----------------------------------------------------------------------===//

#include "flang/Sema/Scope.h"
using namespace flang;

void Scope::Init(Scope *parent, unsigned flags) {
  AnyParent = parent;
  Flags = flags;

  if (parent) {
    Depth          = parent->Depth + 1;
    PrototypeDepth = parent->PrototypeDepth;
    PrototypeIndex = 0;
    FnParent       = parent->FnParent;
  } else {
    Depth = 0;
    PrototypeDepth = 0;
    PrototypeIndex = 0;
    FnParent = 0;
  }

  // If this scope is a function or contains breaks/continues, remember it.
  if (flags & FnScope) FnParent = this;

  DeclsInScope.clear();
  Entity = 0;
  ErrorTrap.reset();
}
