//===--- TextDiagnosticPrinter.h - Text Diagnostic Client -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a concrete diagnostic client, which prints the diagnostics to
// standard error.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_FRONTEND_TEXT_DIAGNOSTIC_PRINTER_H_
#define FLANG_FRONTEND_TEXT_DIAGNOSTIC_PRINTER_H_

#include "flang/Basic/Diagnostic.h"

namespace llvm {
  class raw_ostream;
  class SMLoc;
  class SourceMgr;
} // end namespace llvm

namespace flang {

class LangOptions;

class TextDiagnosticPrinter : public DiagnosticClient {
  llvm::SourceMgr &SrcMgr;
public:
  TextDiagnosticPrinter(llvm::SourceMgr &SM) : SrcMgr(SM) {}
  virtual ~TextDiagnosticPrinter();

  // TODO: Emit caret diagnostics and Highlight range.
  virtual void HandleDiagnostic(Diagnostic::Level DiagLevel, llvm::SMLoc L,
                                const llvm::Twine &Msg);
};

} // end namespace flang

#endif
