//===--- Sema.h - Semantic Analysis & AST Building --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Sema class, which performs semantic analysis and builds
// ASTs.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_SEMA_SEMA_H__
#define FLANG_SEMA_SEMA_H__

#include "flang/Basic/Token.h"
#include "flang/AST/FormatSpec.h"
#include "flang/AST/Stmt.h"
#include "flang/AST/Type.h"
#include "flang/Sema/Ownership.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SMLoc.h"
#include "flang/Basic/LLVM.h"

namespace flang {

class ASTContext;
class DeclContext;
class DeclSpec;
class DeclarationNameInfo;
class Diagnostic;
class Expr;
class FormatSpec;
class IdentifierInfo;
class Token;
class VarDecl;

/// Sema - This implements semantic analysis and AST buiding for Fortran.
class Sema {
  Sema(const Sema&);           // DO NOT IMPLEMENT
  void operator=(const Sema&); // DO NOT IMPLEMENT
public:
  typedef Expr ExprTy;

  ASTContext &Context;
  Diagnostic &Diags;

  /// CurContext - This is the current declaration context of parsing.
  DeclContext *CurContext;

  Sema(ASTContext &ctxt, Diagnostic &Diags);
  ~Sema();

  DeclContext *getContainingDC(DeclContext *DC);

  /// Set the current declaration context until it gets popped.
  void PushDeclContext(DeclContext *DC);
  void PopDeclContext();

  void ActOnTranslationUnit();
  void ActOnEndProgramUnit();

  void ActOnMainProgram(const IdentifierInfo *IDInfo, SMLoc NameLoc);
  void ActOnEndMainProgram(const IdentifierInfo *IDInfo, SMLoc NameLoc);

  QualType ActOnTypeName(ASTContext &C, DeclSpec &DS);
  VarDecl *ActOnKindSelector(ASTContext &C, SMLoc IDLoc,
                             const IdentifierInfo *IDInfo);
  Decl *ActOnEntityDecl(ASTContext &C, DeclSpec &DS, SMLoc IDLoc,
                        const IdentifierInfo *IDInfo);

  StmtResult ActOnPROGRAM(ASTContext &C, const IdentifierInfo *ProgName,
                          SMLoc Loc, SMLoc NameLoc, Expr *StmtLabel);
  StmtResult ActOnIMPLICIT(ASTContext &C, DeclSpec &DS,
                        ArrayRef<std::pair<const IdentifierInfo*,
                                           const IdentifierInfo*> > LetterSpecs,
                           Expr *StmtLabel);
  StmtResult ActOnIMPLICIT(ASTContext &C, Expr *StmtLabel);
  StmtResult ActOnPARAMETER(llvm::ArrayRef<std::pair<const IdentifierInfo*,
                                           ExprResult> > NamedConsts,
                            Expr *StmtLabel);
  StmtResult ActOnENDPROGRAM(ASTContext &C,
                             const IdentifierInfo *ProgName,
                             SMLoc Loc, SMLoc NameLoc,
                             Expr *StmtLabel);

  StmtResult ActOnAssignmentStmt(ASTContext &C, ExprResult LHS,
                                 ExprResult RHS, Expr *StmtLabel);

  QualType ActOnArraySpec(ASTContext &C, QualType ElemTy,
                          ArrayRef<ExprResult> Dims);

  StarFormatSpec *ActOnStarFormatSpec(ASTContext &C, SMLoc Loc);
  DefaultCharFormatSpec *ActOnDefaultCharFormatSpec(ASTContext &C,
                                                    SMLoc Loc,
                                                    ExprResult Fmt);
  LabelFormatSpec *ActOnLabelFormatSpec(ASTContext &C, SMLoc Loc,
                                        ExprResult Label);

  StmtResult ActOnPrintStmt(ASTContext &C, SMLoc Loc, FormatSpec *FS,
                            ArrayRef<ExprResult> OutputItemList,
                            Expr *StmtLabel);

  // FIXME: TODO:

  StmtResult ActOnIMPORT(llvm::ArrayRef<IdentifierInfo*>,
                         Expr *StmtLabel) { return StmtResult(); }
  StmtResult ActOnUSE(UseStmt::ModuleNature MN, llvm::StringRef Name,
                      bool OnlyList,
                      llvm::ArrayRef<const VarDecl*> LocalNames,
                      llvm::ArrayRef<const VarDecl*> UseNames,
                      Expr *StmtLabel) { return StmtResult(); }
  StmtResult
  ActOnASYNCHRONOUS(llvm::ArrayRef<const IdentifierInfo*> ObjNames,
                    Expr *StmtLabel) { return StmtResult(); }

  QualType ActOnBuiltinType(ASTContext *Ctx,
                            BuiltinType::TypeSpec TS,
                            Expr *Kind) { return QualType(); }
  QualType ActOnCharacterBuiltinType(ASTContext *Ctx,
                                     Expr *Len,
                                     Expr *Kind) { return QualType(); }
  DeclSpec *ActOnTypeDeclSpec(ASTContext *Ctx) { return 0; }

  ExprResult ActOnDataReference(llvm::ArrayRef<ExprResult> Exprs) {
    return ExprResult();
  }
};

} // end flang namespace

#endif
