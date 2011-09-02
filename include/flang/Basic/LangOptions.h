//===--- LangOptions.h - Fortran Language Family Language Opts --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the LangOptions interface.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_LANGOPTIONS_H__
#define FORTRAN_LANGOPTIONS_H__

#include <string>

namespace flang {

/// LangOptions - This class keeps track of the various options that can be
/// enabled, which controls the dialect of Fortran that is accepted.
class LangOptions {
public:
  unsigned Fortran77         : 1; // Fortran 77
  unsigned Fortran90         : 1; // Fortran 90
  unsigned Fortran95         : 1; // Fortran 95
  unsigned Fortran2000       : 1; // Fortran 2000
  unsigned Fortran2003       : 1; // Fortran 2003
  unsigned Fortran2008       : 1; // Fortran 2008

  unsigned FixedForm         : 1; // Fixed-form style
  unsigned FreeForm          : 1; // Free-form style

  unsigned ReturnComments    : 1; // Return comments as lexical tokens

  unsigned SpellChecking     : 1; // Whether to perform spell-checking for error
                                  // recovery.
  LangOptions() {
    Fortran77 = 0;
    Fortran90 = Fortran95 = Fortran2000 = Fortran2003 = 1;
    FixedForm = 0;
    FreeForm = 1;
    ReturnComments = 0;
    SpellChecking = 1;
  }
};

}  // end namespace flang

#endif
