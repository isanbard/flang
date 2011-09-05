//===--- Stmt.h - Fortran Statements ----------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the statement objects.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_AST_STMT_H__
#define FORTRAN_AST_STMT_H__

#include "flang/Basic/Token.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/SMLoc.h"

namespace llvm {
  class StringRef;
}

namespace flang {

class ASTContext;
class IdentifierInfo;

//===----------------------------------------------------------------------===//
/// Stmt - The base class for all Fortran statements.
///
class Stmt {
public:
  enum StmtTy {
    Program,
    Use,
    Import,
    Asynchronous,
    EndProgram
  };
private:
  StmtTy StmtID;
  llvm::SMLoc Loc;
  Token StmtLabelTok;

  Stmt(const Stmt &);           // Do not implement!
  friend class ASTContext;
protected:
  Stmt(StmtTy ID, llvm::SMLoc L, Token SLT)
    : StmtID(ID), Loc(L), StmtLabelTok(SLT) {}
public:
  virtual ~Stmt();

  /// getStatementID - Get the ID of the statement.
  StmtTy getStatementID() const { return StmtID; }

  /// getLocation - Get the location of the statement.
  llvm::SMLoc getLocation() const { return Loc; }

  /// getStmtLabel - Get the statement label for this statement.
  const Token &getStmtLabel() const { return StmtLabelTok; }

  static bool classof(const Stmt*) { return true; }
};

//===----------------------------------------------------------------------===//
/// ProgramStmt - The (optional) first statement of the 'main' program.
///
class ProgramStmt : public Stmt {
  const IdentifierInfo *ProgName;
  llvm::SMLoc NameLoc;

  ProgramStmt(const IdentifierInfo *progName, llvm::SMLoc Loc,
              llvm::SMLoc NameL, Token SLT)
    : Stmt(Program, Loc, SLT), ProgName(progName), NameLoc(NameL) {}
  ProgramStmt(const ProgramStmt &); // Do not implement!
public:
  static ProgramStmt *Create(ASTContext &C, const IdentifierInfo *ProgName,
                             llvm::SMLoc L, llvm::SMLoc NameL,
                             Token StmtLabelTok);

  /// getProgramName - Get the name of the program. This may be null.
  const IdentifierInfo *getProgramName() const { return ProgName; }

  /// getNameLocation - Get the location of the program name.
  llvm::SMLoc getNameLocation() const { return NameLoc; }

  static bool classof(const ProgramStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Program;
  }
};

//===----------------------------------------------------------------------===//
/// EndProgramStmt - The last statement of the 'main' program.
///
class EndProgramStmt : public Stmt {
  const IdentifierInfo *ProgName;

  EndProgramStmt(const IdentifierInfo *progName, Token SLT)
    : Stmt(EndProgram, llvm::SMLoc(), SLT), ProgName(progName) {}
  EndProgramStmt(const EndProgramStmt &); // Do not implement!
public:
  static EndProgramStmt *Create(const IdentifierInfo *ProgName,
                                Token StmtLabelTok);

  /// getProgramName - Get the name of the program. This may be null.
  const IdentifierInfo *getProgramName() const { return ProgName; }

  static bool classof(const EndProgramStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == EndProgram;
  }
};

//===----------------------------------------------------------------------===//
/// UseStmt -
///
class UseStmt : public Stmt {
public:
  enum ModuleNature {
    None,
    Intrinsic,
    NonIntrinsic
  };
private:
  ModuleNature ModNature;
  const IdentifierInfo *ModName;
  bool Only;
  // FIXME: Finish...

  UseStmt(ModuleNature MN, const IdentifierInfo *Info, Token StmtLabelTok);
public:
  static UseStmt *Create(ModuleNature MN, const IdentifierInfo *Info,
                         Token StmtLabelTok);

  /// Accessors:
  ModuleNature getModuleNature() const { return ModNature; }
  llvm::StringRef getModuleName() const;

  static bool classof(const UseStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Use;
  }
};

//===----------------------------------------------------------------------===//
/// ImportStmt - Specifies that the named entities from the host scoping unit
/// are accessible in the interface body by host association.
///
class ImportStmt : public Stmt {
  llvm::SmallVector<const IdentifierInfo*, 4> Names;

  ImportStmt(Token StmtLabelTok);
  ImportStmt(llvm::ArrayRef<const IdentifierInfo*> names, Token StmtLabelTok);
  ImportStmt(const ImportStmt &); // Do not implement!
public:
  static ImportStmt *Create(Token StmtLabelTok);
  static ImportStmt *Create(llvm::ArrayRef<const IdentifierInfo*> Names,
                            Token StmtLabelTok);

  unsigned getNumNames() const { return Names.size(); }

  typedef llvm::SmallVectorImpl<const IdentifierInfo*>::const_iterator iterator;
  iterator begin() const { return Names.begin(); }
  iterator end() const   { return Names.end(); }

  bool empty() const { return Names.empty(); }

  static bool classof(const ImportStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Import;
  }
};

//===----------------------------------------------------------------------===//
/// AsynchronousStmt - Specifies the asynchronous attribute for a list of
/// objects.
///
class AsynchronousStmt : public Stmt {
  llvm::SmallVector<const IdentifierInfo*, 4> ObjNames;
  AsynchronousStmt(llvm::ArrayRef<const IdentifierInfo*> objNames,
                   Token StmtLabelTok);
public:
  static AsynchronousStmt*Create(llvm::ArrayRef<const IdentifierInfo*> objNames,
                                  Token StmtLabelTok);

  static bool classof(const AsynchronousStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Asynchronous;
  }
};


} // end flang namespace

#endif
