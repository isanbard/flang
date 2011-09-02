//===--- SourceLocation.h - Compact identifier for Source Files -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the SourceLocation class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FORTRAN_SOURCELOCATION_H
#define LLVM_FORTRAN_SOURCELOCATION_H

#include "llvm/Support/SMLoc.h"

namespace flang {

/// SourceRange - a trival tuple used to represent a source range.
class SourceRange {
  llvm::SMLoc B;
  llvm::SMLoc E;
public:
  SourceRange(): B(llvm::SMLoc()), E(llvm::SMLoc()) {}
  SourceRange(llvm::SMLoc loc) : B(loc), E(loc) {}
  SourceRange(llvm::SMLoc begin, llvm::SMLoc end) : B(begin), E(end) {}

  llvm::SMLoc getBegin() const { return B; }
  llvm::SMLoc getEnd() const { return E; }

  void setBegin(llvm::SMLoc b) { B = b; }
  void setEnd(llvm::SMLoc e) { E = e; }

  bool isValid() const { return B.isValid() && E.isValid(); }
  bool isInvalid() const { return !isValid(); }

  bool operator==(const SourceRange &that) const {
    return B == that.B && E == that.E;
  }

  bool operator!=(const SourceRange &that) const {
    return !(*this == that);
  }
};

} // end fortran namespace

#endif
