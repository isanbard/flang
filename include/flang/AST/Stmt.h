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

#ifndef FLANG_AST_STMT_H__
#define FLANG_AST_STMT_H__

#include "flang/AST/ASTContext.h"
#include "flang/Basic/Token.h"
#include "flang/Sema/Ownership.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/SMLoc.h"
#include "flang/Basic/LLVM.h"

namespace flang {

class FormatSpec;
class IdentifierInfo;

/// Stmt - The base class for all Fortran statements.
///
class Stmt {
public:
  enum StmtTy {
    Program,
    Use,
    Import,
    Implicit,
    Asynchronous,
    EndProgram,
    Assignment,
    Print
  };
private:
  StmtTy StmtID;
  SMLoc Loc;
  ExprResult StmtLabel;

  Stmt(const Stmt &);           // Do not implement!
  friend class ASTContext;
protected:
  // Make vanilla 'new' and 'delete' illegal for Stmts.
  void* operator new(size_t bytes) throw() {
    assert(0 && "Stmts cannot be allocated with regular 'new'.");
    return 0;
  }
  void operator delete(void* data) throw() {
    assert(0 && "Stmts cannot be released with regular 'delete'.");
  }

  Stmt(StmtTy ID, SMLoc L, ExprResult SLT)
    : StmtID(ID), Loc(L), StmtLabel(SLT) {}
public:
  virtual ~Stmt();

  /// getStatementID - Get the ID of the statement.
  StmtTy getStatementID() const { return StmtID; }

  /// getLocation - Get the location of the statement.
  SMLoc getLocation() const { return Loc; }

  /// getStmtLabel - Get the statement label for this statement.
  ExprResult getStmtLabel() const { return StmtLabel; }

  static bool classof(const Stmt*) { return true; }

public:
  // Only allow allocation of Stmts using the allocator in ASTContext or by
  // doing a placement new.
  void *operator new(size_t bytes, ASTContext &C,
                     unsigned alignment = 8) throw() {
    return ::operator new(bytes, C, alignment);
  }

  void *operator new(size_t bytes, ASTContext *C,
                     unsigned alignment = 8) throw() {
    return ::operator new(bytes, *C, alignment);
  }

  void *operator new(size_t bytes, void *mem) throw() {
    return mem;
  }

  void operator delete(void*, ASTContext&, unsigned) throw() { }
  void operator delete(void*, ASTContext*, unsigned) throw() { }
  void operator delete(void*, std::size_t) throw() { }
  void operator delete(void*, void*) throw() { }
};

/// ProgramStmt - The (optional) first statement of the 'main' program.
///
class ProgramStmt : public Stmt {
  const IdentifierInfo *ProgName;
  SMLoc NameLoc;

  ProgramStmt(const IdentifierInfo *progName, SMLoc Loc,
              SMLoc NameL, ExprResult SLT)
    : Stmt(Program, Loc, SLT), ProgName(progName), NameLoc(NameL) {}
  ProgramStmt(const ProgramStmt &); // Do not implement!
public:
  static ProgramStmt *Create(ASTContext &C, const IdentifierInfo *ProgName,
                             SMLoc L, SMLoc NameL,
                             ExprResult StmtLabel);

  /// getProgramName - Get the name of the program. This may be null.
  const IdentifierInfo *getProgramName() const { return ProgName; }

  /// getNameLocation - Get the location of the program name.
  SMLoc getNameLocation() const { return NameLoc; }

  static bool classof(const ProgramStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Program;
  }
};

/// EndProgramStmt - The last statement of the 'main' program.
///
class EndProgramStmt : public Stmt {
  const IdentifierInfo *ProgName;
  SMLoc NameLoc;

  EndProgramStmt(const IdentifierInfo *progName, SMLoc Loc,
                 SMLoc NameL, ExprResult SLT)
    : Stmt(EndProgram, Loc, SLT), ProgName(progName), NameLoc(NameL) {}
  EndProgramStmt(const EndProgramStmt &); // Do not implement!
public:
  static EndProgramStmt *Create(ASTContext &C, const IdentifierInfo *ProgName,
                                SMLoc L, SMLoc NameL,
                                ExprResult StmtLabel);

  /// getProgramName - Get the name of the program. This may be null.
  const IdentifierInfo *getProgramName() const { return ProgName; }

  /// getNameLocation - Get the location of the program name.
  SMLoc getNameLocation() const { return NameLoc; }

  static bool classof(const EndProgramStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == EndProgram;
  }
};

//===----------------------------------------------------------------------===//
// Specification Part Statements
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
  typedef std::pair<const IdentifierInfo *, const IdentifierInfo *> RenamePair;
private:
  ModuleNature ModNature;
  const IdentifierInfo *ModName;
  bool Only;
  unsigned NumRenames;
  RenamePair *RenameList;

  UseStmt(ASTContext &C, ModuleNature MN, const IdentifierInfo *Info,
          ExprResult StmtLabel, ArrayRef<RenamePair> RenameList);
public:
  static UseStmt *Create(ASTContext &C, ModuleNature MN,
                         const IdentifierInfo *Info,
                         ExprResult StmtLabel,
                         ArrayRef<RenamePair> RenameList);

  /// Accessors:
  ModuleNature getModuleNature() const { return ModNature; }
  StringRef getModuleName() const;

  unsigned getNumRenames() const { return NumRenames; }
  RenamePair getRenamePairAt(unsigned I) {
    assert(I < NumRenames && "Invalid index!");
    return RenameList[I];
  }

  static bool classof(const UseStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Use;
  }
};

/// ImportStmt - Specifies that the named entities from the host scoping unit
/// are accessible in the interface body by host association.
///
class ImportStmt : public Stmt {
  unsigned NumNames;
  const IdentifierInfo **Names;

  ImportStmt(ExprResult StmtLabel);
  ImportStmt(ASTContext &C, ArrayRef<const IdentifierInfo*> names,
             ExprResult StmtLabel);
public:
  static ImportStmt *Create(ASTContext &C, ExprResult StmtLabel);
  static ImportStmt *Create(ASTContext &C,
                            ArrayRef<const IdentifierInfo*> Names,
                            ExprResult StmtLabel);

  unsigned getNumNames() const { return NumNames; }
  const IdentifierInfo *getNameAt(unsigned I) {
    assert(I < NumNames && "Invalid index!");
    return Names[I];
  }

  static bool classof(const ImportStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Import;
  }
};

/// ImplicitStmt - Specifies a type, and possibly type parameters, for all
/// implicitly typed data entries whose names begin with one of the letters
/// specified in the statement.
///
class ImplicitStmt : public Stmt {
public:
  typedef std::pair<const IdentifierInfo *, const IdentifierInfo *> LetterSpec;
private:
  QualType Ty;
  bool None;
  unsigned NumLetterSpecs;
  LetterSpec *LetterSpecList;

  ImplicitStmt(SMLoc L, ExprResult StmtLabel);
  ImplicitStmt(ASTContext &C, SMLoc L, QualType T,
               ArrayRef<LetterSpec> SpecList, ExprResult StmtLabel);
public:
  static ImplicitStmt *Create(ASTContext &C, SMLoc L, ExprResult StmtLabel);
  static ImplicitStmt *Create(ASTContext &C, SMLoc L, QualType T,
                              ArrayRef<LetterSpec> SpecList,
                              ExprResult StmtLabel);

  unsigned getNumLetterSpecs() const { return NumLetterSpecs; }
  LetterSpec getLetterSpecAt(unsigned I) {
    assert(I < NumLetterSpecs && "Invalid index!");
    return LetterSpecList[I];
  }

  static bool classof(const ImplicitStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Implicit;
  }
};

/// AsynchronousStmt - Specifies the asynchronous attribute for a list of
/// objects.
///
class AsynchronousStmt : public Stmt {
  unsigned NumObjNames;
  const IdentifierInfo **ObjNames;
  AsynchronousStmt(ASTContext &C, ArrayRef<const IdentifierInfo*> objNames,
                   ExprResult StmtLabel);
public:
  static AsynchronousStmt *Create(ASTContext &C,
                                  ArrayRef<const IdentifierInfo*> objNames,
                                  ExprResult StmtLabel);

  unsigned getNumObjectNames() const { return NumObjNames; }
  const IdentifierInfo *getObjectNameAt(unsigned I) const {
    assert(I < NumObjNames && "Invalid index!");
    return ObjNames[I];
  }

  static bool classof(const AsynchronousStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Asynchronous;
  }
};

//===----------------------------------------------------------------------===//
// Executable Statements
//===----------------------------------------------------------------------===//

/// AssignmentStmt
class AssignmentStmt : public Stmt {
  ExprResult LHS;
  ExprResult RHS;

  AssignmentStmt(ExprResult LHS, ExprResult RHS, ExprResult StmtLabel);
public:
  static AssignmentStmt *Create(ASTContext &C, ExprResult LHS,
                                ExprResult RHS, ExprResult StmtLabel);

  Expr *getLHS() const { return LHS.get(); }
  Expr *getRHS() const { return RHS.get(); }

  static bool classof(const AssignmentStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Assignment;
  }
};

/// PrintStmt
class PrintStmt : public Stmt {
  FormatSpec *FS;
  unsigned NumOutputItems;
  ExprResult *OutputItemList;

  PrintStmt(ASTContext &C, SMLoc L, FormatSpec *fs,
            ArrayRef<ExprResult> OutList, ExprResult StmtLabel);
public:
  static PrintStmt *Create(ASTContext &C, SMLoc L, FormatSpec *fs,
                           ArrayRef<ExprResult> OutList, ExprResult StmtLabel);

  FormatSpec *getFormatSpec() const { return FS; }
  ArrayRef<ExprResult> getOutputItemList() const {
    return ArrayRef<ExprResult>(OutputItemList, NumOutputItems);
  }

  static bool classof(const PrintStmt*) { return true; }
  static bool classof(const Stmt *S) {
    return S->getStatementID() == Print;
  }
};

} // end flang namespace

#endif
