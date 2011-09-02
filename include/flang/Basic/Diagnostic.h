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

namespace flang {

class DiagnosticClient;
class DiagnosticErrorTrap;
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
  DiagnosticClient *Client;
  bool OwnsDiagClient;
  llvm::SourceMgr *SrcMgr;

  /// \brief Counts for DiagnosticErrorTrap to check whether an error occurred
  /// during a parsing section, e.g. during parsing a function.
  unsigned TrapNumErrorsOccurred;
  unsigned TrapNumUnrecoverableErrorsOccurred;
public:
  Diagnostic(llvm::SourceMgr *SM, DiagnosticClient *DC,
             bool ShouldOwnClient = true)
    : Client(DC), OwnsDiagClient(ShouldOwnClient), SrcMgr(SM) {}

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

private:
  // This is private state used by DiagnosticBuilder. We put it here instead of
  // in DiagnosticBuilder in order to keep DiagnosticBuilder a small lightweight
  // object. This implementation choice means that we can only have one
  // diagnostic "in flight" at a time, but this seems to be a reasonable
  // tradeoff to keep these objects small. Assertions verify that only one
  // diagnostic is in flight at a time.
  friend class DiagnosticErrorTrap;
};

/// \brief RAII class that determines when any errors have occurred between the
/// time the instance was created and the time it was queried.
class DiagnosticErrorTrap {
  Diagnostic &Diag;
  unsigned NumErrors;
  unsigned NumUnrecoverableErrors;
public:
  explicit DiagnosticErrorTrap(Diagnostic &Diag)
    : Diag(Diag) { reset(); }

  /// \brief Determine whether any errors have occurred since this
  /// object instance was created.
  bool hasErrorOccurred() const {
    return Diag.TrapNumErrorsOccurred > NumErrors;
  }

  /// \brief Determine whether any unrecoverable errors have occurred since this
  /// object instance was created.
  bool hasUnrecoverableErrorOccurred() const {
    return Diag.TrapNumUnrecoverableErrorsOccurred > NumUnrecoverableErrors;
  }

  // Set to initial state of "no errors occurred".
  void reset() {
    NumErrors = Diag.TrapNumErrorsOccurred;
    NumUnrecoverableErrors = Diag.TrapNumUnrecoverableErrorsOccurred;
  }
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

} // end namespace flang

#endif
