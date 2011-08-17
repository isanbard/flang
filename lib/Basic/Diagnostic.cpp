//===--- Diagnostic.cpp - Fortran Language Diagnostic Handling ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Diagnostic-related interfaces.
//
//===----------------------------------------------------------------------===//

#include "flang/Basic/Diagnostic.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/Twine.h"
using namespace fortran;

bool Diagnostic::hadErrors() {
  return Client->getNumErrors() != 0;
}

bool Diagnostic::hadWarnings() {
  return Client->getNumWarnings() != 0;
}

/// ReportError - Emit an error at the location \arg L, with the message \arg
/// Msg.
///
/// \return The return value is always true, as an idiomatic convenience to
/// clients.
bool Diagnostic::ReportError(llvm::SMLoc L, const llvm::Twine &Msg) {
  Client->HandleDiagnostic(Error, L, Msg);
  return true;
}

/// ReportWarning - Emit a warning at the location \arg L, with the message \arg
/// Msg.
///
/// \return The return value is always true, as an idiomatic convenience to
/// clients.
bool Diagnostic::ReportWarning(llvm::SMLoc L, const llvm::Twine &Msg) {
  Client->HandleDiagnostic(Warning, L, Msg);
  return true;
}

DiagnosticClient::~DiagnosticClient() {}

void DiagnosticClient::HandleDiagnostic(Diagnostic::Level DiagLevel,
                                        llvm::SMLoc,
                                        const llvm::Twine &) {
  if (!IncludeInDiagnosticCounts())
    return;

  if (DiagLevel == Diagnostic::Warning)
    ++NumWarnings;
  else if (DiagLevel >= Diagnostic::Error)
    ++NumErrors;
}
