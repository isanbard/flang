//===--- StmtDumper.h - Dump Fortran Statements -----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the functions that dump statements.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_AST_STMTDUMPER_H__
#define FLANG_AST_STMTDUMPER_H__

#include "llvm/ADT/ArrayRef.h"

namespace flang {
  class Stmt;

  /// dump - Dump an array of statements.
  void dump(llvm::ArrayRef<Stmt*> S);

} // end flang namespace

#endif
