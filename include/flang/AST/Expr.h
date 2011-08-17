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

#ifndef FORTRAN_AST_EXPR_H__
#define FORTRAN_AST_EXPR_H__

#include "flang/Sema/Ownership.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/StringRef.h"

namespace fortran {

class IdentifierInfo;
class VarDecl;

class Expr {
public:
  enum ExprType {
    // Unary Expressions
    ET_Constant,
    ET_Variable,
    ET_Unary,
    ET_DefinedUnaryOperator,

    // Binary Expressions
    ET_Binary,
    ET_DefinedBinaryOperator
  };
private:
  ExprType ExprID;
  llvm::SMLoc Loc;
public:
  Expr(ExprType ET, llvm::SMLoc L) : ExprID(ET), Loc(L) {}
  virtual ~Expr();

  ExprType getExpressionID() const { return ExprID; }
  llvm::SMLoc getLocation() const { return Loc; }

  virtual void print(llvm::raw_ostream&);
  void dump();

  static bool classof(const Expr *) { return true; }
};

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
  UnaryExpr(ExprType ET, llvm::SMLoc loc, Operator op, ExprResult e)
    : Expr(ET, loc), Op(op), E(e) {}
public:
  UnaryExpr(llvm::SMLoc loc, Operator op, ExprResult e)
    : Expr(Expr::ET_Unary, loc), Op(op), E(e) {}
  virtual ~UnaryExpr();

  Operator getOperator() const { return Op; }

  const ExprResult getExpression() const { return E; }
  ExprResult getExpression() { return E; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::ET_Unary;
  }
  static bool classof(const UnaryExpr *) { return true; }
};

class DefinedOperatorUnaryExpr : public UnaryExpr {
  IdentifierInfo *II;
public:
  DefinedOperatorUnaryExpr(llvm::SMLoc loc, ExprResult e, IdentifierInfo *ii)
    : UnaryExpr(ET_DefinedUnaryOperator, loc, Defined, e), II(ii) {}

  const IdentifierInfo *getIdentifierInfo() const { return II; }
  IdentifierInfo *getIdentifierInfo() { return II; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::ET_DefinedUnaryOperator;
  }
  static bool classof(const DefinedOperatorUnaryExpr *) { return true; }
};

class ConstantExpr : public Expr {
  llvm::StringRef Data;
public:
  ConstantExpr(llvm::SMLoc loc, llvm::StringRef data)
    : Expr(ET_Constant, loc), Data(data) {}

  llvm::StringRef getData() const { return Data; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::ET_Constant;
  }
  static bool classof(const ConstantExpr *) { return true; }
};

class VarExpr : public Expr {
  const VarDecl *Variable;
public:
  VarExpr(llvm::SMLoc Loc, const VarDecl *Var)
    : Expr(ET_Variable, Loc), Variable(Var) {}

  const VarDecl *getVarDecl() const { return Variable; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::ET_Variable;
  }
  static bool classof(const ConstantExpr *) { return true; }
};

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
    : Expr(ET_Binary, loc), Op(op), LHS(lhs), RHS(rhs) {}
  virtual ~BinaryExpr();

  Operator getOperator() const { return Op; }

  const ExprResult getLHS() const { return LHS; }
  ExprResult getLHS() { return LHS; }
  const ExprResult getRHS() const { return RHS; }
  ExprResult getRHS() { return RHS; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::ET_Binary;
  }
  static bool classof(const BinaryExpr *) { return true; }
};

class DefinedOperatorBinaryExpr : public BinaryExpr {
  IdentifierInfo *II;
public:
  DefinedOperatorBinaryExpr(llvm::SMLoc loc, ExprResult lhs, ExprResult rhs,
                            IdentifierInfo *ii)
    : BinaryExpr(ET_DefinedBinaryOperator, loc, Defined, lhs, rhs), II(ii) {}

  const IdentifierInfo *getIdentifierInfo() const { return II; }
  IdentifierInfo *getIdentifierInfo() { return II; }

  virtual void print(llvm::raw_ostream&);

  static bool classof(const Expr *E) {
    return E->getExpressionID() == Expr::ET_DefinedBinaryOperator;
  }
  static bool classof(const DefinedOperatorBinaryExpr *) { return true; }
};

} // end fortran namespace

#endif
