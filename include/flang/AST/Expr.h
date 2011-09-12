//===--- Expr.h - Fortran Expressions ---------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the expression objects.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_AST_EXPR_H__
#define FLANG_AST_EXPR_H__

#include "flang/AST/Type.h"
#include "flang/Sema/Ownership.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringRef.h"
#include "flang/Basic/LLVM.h"

namespace flang {

class ASTContext;
class IdentifierInfo;
class Decl;
class VarDecl;

//===----------------------------------------------------------------------===//
/// Expr -
class Expr {
protected:
  enum ExprType {
    // Primary Expressions
    Designator,

    // Unary Expressions
    Constant,
    IntegerConstant,
    RealConstant,
    BOZConstant,
    LogicalConstant,

    Variable,
    Unary,
    DefinedUnaryOperator,

    // Binary Expressions
    Binary,
    DefinedBinaryOperator
  };
private:
  ExprType ExprID;
  llvm::SMLoc Loc;
  friend class ASTContext;
public:
  Expr(ExprType ET, llvm::SMLoc L) : ExprID(ET), Loc(L) {}
  virtual ~Expr();

  ExprType getExpressionID() const { return ExprID; }
  llvm::SMLoc getLocation() const { return Loc; }

  virtual void print(raw_ostream&);
  void dump();

  static bool classof(const Expr *) { return true; }
};

//===----------------------------------------------------------------------===//
/// ConstantExpr -
class ConstantExpr : public Expr {
  StringRef Data;
  std::string Kind;         // Optional Kind Selector
protected:
  ConstantExpr(ExprType Ty, llvm::SMLoc Loc)
    : Expr(Ty, Loc) {}
public:
  ConstantExpr(llvm::SMLoc loc, llvm::StringRef data)
    : Expr(Expr::Constant, loc), Data(data) {}

  llvm::StringRef getData() const { return Data; }

  const std::string &getKindSelector() const { return Kind; }
  void setKindSelector(llvm::StringRef K) { Kind = K; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    ExprType ETy = E->getExpressionID();
    return ETy == Expr::Constant ||
      ETy == Expr::IntegerConstant || ETy == Expr::RealConstant ||
      ETy == Expr::BOZConstant || ETy == Expr::LogicalConstant;
  }
  static bool classof(const ConstantExpr *) { return true; }
};

class IntegerConstantExpr : public ConstantExpr {
  APInt Val;
  IntegerConstantExpr(SMLoc Loc, StringRef Data);
public:
  static IntegerConstantExpr *Create(ASTContext &C, SMLoc Loc,
                                     StringRef Data);

  const APInt &getValue() const { return Val; }

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::IntegerConstant;
  }
  static bool classof(const IntegerConstantExpr *) { return true; }
};

class RealConstantExpr : public ConstantExpr {
  APFloat Val;
  RealConstantExpr(llvm::SMLoc Loc, llvm::StringRef Data);
public:
  static RealConstantExpr *Create(ASTContext &C, SMLoc Loc, StringRef Data);

  const APFloat &getValue() const { return Val; }

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::RealConstant;
  }
  static bool classof(const RealConstantExpr *) { return true; }
};

class BOZConstantExpr : public ConstantExpr {
public:
  enum BOZKind { Hexadecimal, Octal, Binary };
private:
  APInt Val;
  BOZKind Kind;
  BOZConstantExpr(llvm::SMLoc Loc, llvm::StringRef Data);
public:
  static BOZConstantExpr *Create(ASTContext &C, llvm::SMLoc Loc,
                                 llvm::StringRef Data);

  BOZKind getBOZKind() const { return Kind; }

  bool isBinaryKind() const { return Kind == Binary; }
  bool isOctalKind() const { return Kind == Octal; }
  bool isHexKind() const { return Kind == Hexadecimal; }

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::BOZConstant;
  }
  static bool classof(const BOZConstantExpr *) { return true; }
};

class LogicalConstantExpr : public ConstantExpr {
  bool Val;
  LogicalConstantExpr(llvm::SMLoc Loc, llvm::StringRef Data);
public:
  static LogicalConstantExpr *Create(ASTContext &C, llvm::SMLoc Loc,
                                     llvm::StringRef Data);

  bool isTrue() const { return Val; }
  bool isFalse() const { return !Val; }

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::LogicalConstant;
  }
  static bool classof(const LogicalConstantExpr *) { return true; }
};

//===----------------------------------------------------------------------===//
/// DesignatorExpr -
class DesignatorExpr : public Expr {
public:
  enum DesignatorTy {
    ObjectName,
    ArrayElement,
    ArraySection,
    CoindexedNamedObject,
    ComplexPartDesignator,
    StructureComponent,
    Substring
  };
private:
  DesignatorTy Ty;
  DesignatorExpr(ExprType ET, llvm::SMLoc loc, DesignatorTy ty)
    : Expr(ET, loc), Ty(ty) {}
public:
  DesignatorExpr(llvm::SMLoc loc, DesignatorTy ty)
    : Expr(Expr::Designator, loc), Ty(ty) {}
  virtual ~DesignatorExpr();

  DesignatorTy getDesignatorType() const { return Ty; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::Designator;
  }
  static bool classof(const DesignatorExpr *) { return true; }
};

////////////////////////////////////////////////////////////////////////////////
// FIXME: Should this go somewhere else?

class Subscript {
public:
  enum SubscriptTy { Normal = 0, EmptyRange = ':', Splat = '*' };
private:
  SubscriptTy Ty;
  Expr *Val;
  Subscript(SubscriptTy ty, Expr *e = 0)
    : Ty(ty), Val(e) {}
  Subscript(const Subscript&);  // Don't implement.
public:
  virtual ~Subscript();

  static Subscript *create(Expr *E);
  static Subscript *createEmptyRange();
  static Subscript *createSplat();

  Expr *getValue() const { return Val; }
  void setValue(Expr *V) { Val = V; }

  bool isEmptyRange() const { return Ty == EmptyRange; }
  bool isSplat() const { return Ty == Splat; }
};

class SubscriptTriplet : public Subscript {
  Subscript *Sub1;
  Subscript *Sub2;
  Subscript *Stride;
public:
};

class VectorSubscript : public Subscript {
  // int-expr
public:
};

class CoSubscript : public Subscript {
public:
};

class PartRef {
  Decl *PartName;
  llvm::SmallVector<Subscript*, 4> Subscripts;
  llvm::SmallVector<CoSubscript*, 2> ImageSelector;
public:
};

//
////////////////////////////////////////////////////////////////////////////////

//===----------------------------------------------------------------------===//
/// VarExpr -
class VarExpr : public DesignatorExpr {
  const VarDecl *Variable;
public:
  VarExpr(llvm::SMLoc Loc, const VarDecl *Var)
    : DesignatorExpr(Loc, DesignatorExpr::ObjectName), Variable(Var) {}

  const VarDecl *getVarDecl() const { return Variable; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::Designator &&
      llvm::cast<DesignatorExpr>(E)->getDesignatorType() ==
      DesignatorExpr::ObjectName;
  }
  static bool classof(const DesignatorExpr *E) {
    return E->getDesignatorType() == DesignatorExpr::ObjectName;
  }
  static bool classof(const VarExpr *) { return true; }
};

//===----------------------------------------------------------------------===//
/// UnaryExpr -
class UnaryExpr : public Expr {
public:
  enum Operator {
    None,
    // Level-5 operand.
    Not,

    // Level-2 operands.
    Plus,
    Minus,

    // Level-1 operand.
    Defined
  };
protected:
  Operator Op;
  ExprResult E;
  UnaryExpr(ExprType ET, SMLoc loc, Operator op, ExprResult e)
    : Expr(ET, loc), Op(op), E(e) {}
public:
  static UnaryExpr *Create(ASTContext &C, SMLoc loc, Operator op, ExprResult e);

  Operator getOperator() const { return Op; }

  const ExprResult getExpression() const { return E; }
  ExprResult getExpression() { return E; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::Unary;
  }
  static bool classof(const UnaryExpr *) { return true; }
};

//===----------------------------------------------------------------------===//
/// DefinedOperatorUnaryExpr -
class DefinedOperatorUnaryExpr : public UnaryExpr {
  IdentifierInfo *II;
  DefinedOperatorUnaryExpr(SMLoc loc, ExprResult e, IdentifierInfo *ii)
    : UnaryExpr(Expr::DefinedUnaryOperator, loc, Defined, e), II(ii) {}
public:
  static DefinedOperatorUnaryExpr *Create(ASTContext &C, SMLoc loc,
                                          ExprResult e, IdentifierInfo *ii);

  const IdentifierInfo *getIdentifierInfo() const { return II; }
  IdentifierInfo *getIdentifierInfo() { return II; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::DefinedUnaryOperator;
  }
  static bool classof(const DefinedOperatorUnaryExpr *) { return true; }
};

//===----------------------------------------------------------------------===//
/// BinaryExpr -
class BinaryExpr : public Expr {
public:
  enum Operator {
    None,

    // Level-5 operators
    Eqv,
    Neqv,
    Or,
    And,
    Defined,

    // Level-4 operators
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    // Level-3 operator
    Concat,

    // Level-2 operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Power
  };
protected:
  Operator Op;
  ExprResult LHS;
  ExprResult RHS;
  BinaryExpr(ExprType ET, llvm::SMLoc loc, Operator op,
             ExprResult lhs, ExprResult rhs)
    : Expr(ET, loc), Op(op), LHS(lhs), RHS(rhs) {}
public:
  BinaryExpr(llvm::SMLoc loc, Operator op, ExprResult lhs, ExprResult rhs)
    : Expr(Expr::Binary, loc), Op(op), LHS(lhs), RHS(rhs) {}
  virtual ~BinaryExpr();

  Operator getOperator() const { return Op; }

  const ExprResult getLHS() const { return LHS; }
  ExprResult getLHS() { return LHS; }
  const ExprResult getRHS() const { return RHS; }
  ExprResult getRHS() { return RHS; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::Binary;
  }
  static bool classof(const BinaryExpr *) { return true; }
};

//===----------------------------------------------------------------------===//
/// DefinedOperatorBinaryExpr -
class DefinedOperatorBinaryExpr : public BinaryExpr {
  IdentifierInfo *II;
public:
  DefinedOperatorBinaryExpr(llvm::SMLoc loc, ExprResult lhs, ExprResult rhs,
                            IdentifierInfo *ii)
    : BinaryExpr(Expr::DefinedBinaryOperator, loc, Defined, lhs, rhs), II(ii) {}

  const IdentifierInfo *getIdentifierInfo() const { return II; }
  IdentifierInfo *getIdentifierInfo() { return II; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::DefinedBinaryOperator;
  }
  static bool classof(const DefinedOperatorBinaryExpr *) { return true; }
};

} // end flang namespace

#endif
