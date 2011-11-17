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

#include "flang/Sema/Ownership.h"
#include "llvm/ADT/ArrayRef.h"

namespace flang {

  /// dump - Dump a statement.
  void dump(StmtResult S);

  /// dump - Dump an array of statements.
  void dump(llvm::ArrayRef<StmtResult> S);

} // end flang namespace

#endif
