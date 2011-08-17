//===--- Diagnostic.h - Fortran Language Diagnostic Handling ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Diagnostic-related interfaces.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_DIAGNOSTIC_H__
#define FORTRAN_DIAGNOSTIC_H__

#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"

namespace llvm {
  class SourceMgr;
  class Twine;
} // end namespace llvm

namespace fortran {

class DiagnosticClient;
class LangOptions;

/// Diagnostic - This concrete class is used by the front-end to report problems
/// and issues. It manages the diagnostics and passes them off to the
/// DiagnosticClient for reporting to the user.
class Diagnostic : public llvm::RefCountedBase<Diagnostic> {
public:
  /// Level - The level of the diagnostic, after it has been through mapping.
  enum Level {
    Ignored = 0,
    Note    = 1,
    Warning = 2,
    Error   = 3,
    Fatal   = 4
  };
private:
  llvm::SourceMgr *SrcMgr;
  DiagnosticClient *Client;
  bool OwnsDiagClient;
public:
  Diagnostic(llvm::SourceMgr *SM, DiagnosticClient *DC,
             bool ShouldOwnClient = true) :
    SrcMgr(SM), Client(DC), OwnsDiagClient(ShouldOwnClient) {}

  DiagnosticClient *getClient() { return Client; }
  const DiagnosticClient *getClient() const { return Client; }

  bool hadErrors();
  bool hadWarnings();

  /// \brief Return the current diagnostic client along with ownership of that
  /// client.
  DiagnosticClient *takeClient() {
    OwnsDiagClient = false;
    return Client;
  }

  bool hasSourceManager() const { return SrcMgr != 0; }
  llvm::SourceMgr &getSourceManager() const {
    assert(SrcMgr && "SourceManager not set!");
    return *SrcMgr;
  }
  void setSourceManager(llvm::SourceMgr *SM) { SrcMgr = SM; }

  /// \brief Set the diagnostic client associated with this diagnostic object.
  ///
  /// \param ShouldOwnClient true if the diagnostic object should take
  /// ownership of \c client.
  void setClient(DiagnosticClient *client, bool ShouldOwnClient = true) {
    Client = client;
    OwnsDiagClient = ShouldOwnClient;
  }

  /// ReportError - Emit an error at the location \arg L, with the message \arg
  /// Msg.
  ///
  /// \return The return value is always true, as an idiomatic convenience to
  /// clients.
  bool ReportError(llvm::SMLoc L, const llvm::Twine &Msg);

  /// ReportWarning - Emit a warning at the location \arg L, with the message
  /// \arg Msg.
  ///
  /// \return The return value is always true, as an idiomatic convenience to
  /// clients.
  bool ReportWarning(llvm::SMLoc L, const llvm::Twine &Msg);
};

/// DiagnosticClient - This is an abstract interface implemented by clients of
/// the front-end, which formats and prints fully processed diagnostics.
class DiagnosticClient {
protected:
  unsigned NumWarnings;       // Number of warnings reported
  unsigned NumErrors;         // Number of errors reported
public:
  DiagnosticClient() : NumWarnings(0), NumErrors(0) { }

  unsigned getNumErrors() const { return NumErrors; }
  unsigned getNumWarnings() const { return NumWarnings; }

  virtual ~DiagnosticClient();

  /// BeginSourceFile - Callback to inform the diagnostic client that processing
  /// of a source file is beginning.
  ///
  /// Note that diagnostics may be emitted outside the processing of a source
  /// file, for example during the parsing of command line options. However,
  /// diagnostics with source range information are required to be emitted only
  /// in between BeginSourceFile() and EndSourceFile().
  ///
  /// \arg LO - The language options for the source file being processed.
  virtual void BeginSourceFile(const LangOptions &) {}

  /// EndSourceFile - Callback to inform the diagnostic client that processing
  /// of a source file has ended. The diagnostic client should assume that any
  /// objects made available via \see BeginSourceFile() are inaccessible.
  virtual void EndSourceFile() {}

  /// IncludeInDiagnosticCounts - This method (whose default implementation
  /// returns true) indicates whether the diagnostics handled by this
  /// DiagnosticClient should be included in the number of diagnostics reported
  /// by Diagnostic.
  virtual bool IncludeInDiagnosticCounts() const { return true; }

  /// HandleDiagnostic - Handle this diagnostic, reporting it to the user or
  /// capturing it to a log as needed.
  ///
  /// Default implementation just keeps track of the total number of warnings
  /// and errors.
  virtual void HandleDiagnostic(Diagnostic::Level DiagLevel, llvm::SMLoc L,
                                const llvm::Twine &Msg);
};

} // end namespace fortran

#endif
