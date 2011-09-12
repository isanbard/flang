//===--- TextDiagnosticPrinter.cpp - Diagnostic Printer -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This diagnostic client prints out their diagnostic messages.
//
//===----------------------------------------------------------------------===//

#include "flang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include <algorithm>
using namespace flang;

static const enum llvm::raw_ostream::Colors noteColor =
  llvm::raw_ostream::BLACK;
static const enum llvm::raw_ostream::Colors fixitColor =
  llvm::raw_ostream::GREEN;
static const enum llvm::raw_ostream::Colors caretColor =
  llvm::raw_ostream::GREEN;
static const enum llvm::raw_ostream::Colors warningColor =
  llvm::raw_ostream::MAGENTA;
static const enum llvm::raw_ostream::Colors errorColor = llvm::raw_ostream::RED;
static const enum llvm::raw_ostream::Colors fatalColor = llvm::raw_ostream::RED;
// Used for changing only the bold attribute.
static const enum llvm::raw_ostream::Colors savedColor =
  llvm::raw_ostream::SAVEDCOLOR;

/// \brief Number of spaces to indent when word-wrapping.
const unsigned WordWrapIndentation = 6;

TextDiagnosticPrinter::~TextDiagnosticPrinter() {}

void TextDiagnosticPrinter::HandleDiagnostic(Diagnostic::Level Level,
                                             llvm::SMLoc L,
                                             const llvm::Twine &Msg) {
  // Default implementation (Warnings/errors count).
  DiagnosticClient::HandleDiagnostic(Level, L, Msg);
  const char *MsgTy = 0;
  switch (Level) {
  case Diagnostic::Ignored: return;
  case Diagnostic::Note:    MsgTy = "note";    break;
  case Diagnostic::Warning: MsgTy = "warning"; break;
  case Diagnostic::Error:   MsgTy = "error";   break;
  case Diagnostic::Fatal:   MsgTy = "fatal";   break;
  }

  SrcMgr.PrintMessage(L, Msg, MsgTy);
}
