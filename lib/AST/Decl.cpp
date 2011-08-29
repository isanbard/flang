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
using namespace fortran;

Decl::~Decl() {}

DeclContext::~DeclContext() {}
