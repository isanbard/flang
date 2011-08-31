//===-- Actions.h - Actions Invoked by Parser -------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Actions invoked by the parser.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_ACTIONS_H__
#define FORTRAN_ACTIONS_H__

#include "flang/AST/Stmt.h"
#include "flang/AST/Type.h"
#include "flang/Sema/Ownership.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace llvm {
  template <typename T> class ArrayRef;
  class raw_ostream;
} // end namespace llvm

namespace fortran {

class ASTContext;
class DeclSpec;
class Diagnostic;
class IdentifierInfo;
class Token;
class VarDecl;

/// Action - Abstract virtual base class for the parser actions.
class Action {
public:
  virtual ~Action();
  virtual StmtResult ActOnPROGRAM(const IdentifierInfo *ProgName,
                                  Token &StmtLabel)=0;
  virtual StmtResult ActOnEND_PROGRAM(llvm::SMLoc,
                                      const IdentifierInfo *ProgName,
                                      Token &StmtLabel) = 0;
  virtual StmtResult ActOnIMPORT(llvm::ArrayRef<IdentifierInfo*>,
                                 Token &StmtLabel) = 0;
  virtual StmtResult ActOnIMPLICIT(Token &StmtLabel) = 0;
  virtual StmtResult ActOnUSE(UseStmt::ModuleNature MN, llvm::StringRef Name,
                              bool OnlyList,
                              llvm::ArrayRef<const VarDecl*> LocalNames,
                              llvm::ArrayRef<const VarDecl*> UseNames,
                              Token &StmtLabel) = 0;
  virtual StmtResult
  ActOnPARAMETER(llvm::ArrayRef<const IdentifierInfo*> NamedConsts,
                 llvm::ArrayRef<ExprResult> ConstExprs,
                 Token &StmtLabel) = 0;
  virtual StmtResult
  ActOnASYNCHRONOUS(llvm::ArrayRef<const IdentifierInfo*> ObjNames,
                    Token &StmtLabel) = 0;

  virtual QualType ActOnBuiltinType(ASTContext *Ctx,
                                    BuiltinType::TypeSpec TS,
                                    Selector Kind) = 0;
  virtual QualType ActOnCharacterBuiltinType(ASTContext *Ctx,
                                             Selector Len,
                                             Selector Kind) = 0;
  virtual DeclSpec *ActOnTypeDeclSpec(ASTContext *Ctx) = 0;
  virtual bool ActOnVarDecl(ASTContext *Ctx, DeclSpec *DTS,
                            const VarDecl *VD) = 0;

  virtual bool ActOnArraySpec() = 0;

  virtual ExprResult ActOnDataReference(llvm::ArrayRef<ExprResult> Exprs) = 0;
};

/// PrintAction - This is a default action for the parser. It pretty-prints the
/// program.
class PrintAction : public Action {
  Diagnostic &Diag;
  const IdentifierInfo *ProgramName;
  unsigned Indent;
public:
  PrintAction(Diagnostic &D) : Diag(D), Indent(0) {}
  virtual StmtResult ActOnPROGRAM(const IdentifierInfo *ProgName,
                                  Token &StmtLabel);
  virtual StmtResult ActOnEND_PROGRAM(llvm::SMLoc,
                                      const IdentifierInfo *ProgName,
                                      Token &StmtLabel);
  virtual StmtResult ActOnIMPORT(llvm::ArrayRef<IdentifierInfo*> Names,
                                 Token &StmtLabel);
  virtual StmtResult ActOnIMPLICIT(Token &StmtLabel);
  virtual StmtResult ActOnUSE(UseStmt::ModuleNature MN, llvm::StringRef Name,
                              bool OnlyList,
                              llvm::ArrayRef<const VarDecl*> LocalNames,
                              llvm::ArrayRef<const VarDecl*> UseNames,
                              Token &StmtLabel);
  virtual StmtResult
  ActOnPARAMETER(llvm::ArrayRef<const IdentifierInfo*> NamedConsts,
                 llvm::ArrayRef<ExprResult> ConstExprs,
                 Token &StmtLabel);
  virtual StmtResult
  ActOnASYNCHRONOUS(llvm::ArrayRef<const IdentifierInfo*> ObjNames,
                    Token &StmtLabel);

  virtual QualType ActOnBuiltinType(ASTContext *Ctx,
                                    BuiltinType::TypeSpec TS,
                                    Selector Kind);
  virtual QualType ActOnCharacterBuiltinType(ASTContext *Ctx,
                                             Selector Len,
                                             Selector Kind);
  virtual DeclSpec *ActOnTypeDeclSpec(ASTContext *Ctx);
  virtual bool ActOnVarDecl(ASTContext *Ctx, DeclSpec *DTS,
                            const VarDecl *VD);
  virtual bool ActOnArraySpec();
  virtual ExprResult ActOnDataReference(llvm::ArrayRef<ExprResult> Exprs);
};

} // end fortran namespace

#endif
