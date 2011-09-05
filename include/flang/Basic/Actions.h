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

namespace flang {

class ASTContext;
class DeclSpec;
class DeclarationNameInfo;
class Diagnostic;
class IdentifierInfo;
class Token;
class VarDecl;

/// Action - Abstract virtual base class for the parser actions.
class Action {
public:
  virtual ~Action();
  virtual void ActOnTranslationUnit() = 0;
  virtual void ActOnMainProgram(const DeclarationNameInfo &NameInfo) = 0;
  virtual void ActOneEndMainProgram(const DeclarationNameInfo &EndNameInfo) = 0;
  virtual void ActOnEndProgramUnit() = 0;

  virtual StmtResult ActOnPROGRAM(ASTContext &C, const IdentifierInfo *ProgName,
                                  llvm::SMLoc Loc, llvm::SMLoc NameLoc,
                                  Token &StmtLabel)=0;
  virtual StmtResult ActOnENDPROGRAM(ASTContext &C,
                                     const IdentifierInfo *ProgName,
                                     llvm::SMLoc Loc, llvm::SMLoc NameLoc,
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
                                    Expr *Kind) = 0;
  virtual QualType ActOnCharacterBuiltinType(ASTContext *Ctx,
                                             Expr *Len,
                                             Expr *Kind) = 0;
  virtual DeclSpec *ActOnTypeDeclSpec(ASTContext *Ctx) = 0;
  virtual Decl *ActOnVarDecl(ASTContext *Ctx, DeclSpec *DTS,
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
  virtual void ActOnTranslationUnit();
  virtual void ActOnMainProgram(const DeclarationNameInfo &NameInfo);
  virtual void ActOneEndMainProgram(const DeclarationNameInfo &EndNameInfo);
  virtual void ActOnEndProgramUnit();

  virtual StmtResult ActOnPROGRAM(ASTContext &C, const IdentifierInfo *ProgName,
                                  llvm::SMLoc Loc, llvm::SMLoc NameLoc,
                                  Token &StmtLabel);
  virtual StmtResult ActOnENDPROGRAM(ASTContext &C,
                                     const IdentifierInfo *ProgName,
                                     llvm::SMLoc Loc, llvm::SMLoc NameLoc,
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
                                    Expr *Kind);
  virtual QualType ActOnCharacterBuiltinType(ASTContext *Ctx,
                                             Expr *Len,
                                             Expr *Kind);
  virtual DeclSpec *ActOnTypeDeclSpec(ASTContext *Ctx);
  virtual Decl *ActOnVarDecl(ASTContext *Ctx, DeclSpec *DTS,
                             const VarDecl *VD);
  virtual bool ActOnArraySpec();
  virtual ExprResult ActOnDataReference(llvm::ArrayRef<ExprResult> Exprs);
};

} // end flang namespace

#endif
