//===-- ParserExpr.cpp - Fortran Expression Parser ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Fortran expression parsing.
//
//===----------------------------------------------------------------------===//

#include "flang/Parse/Parser.h"
#include "flang/AST/Decl.h"
#include "flang/AST/Expr.h"
#include "flang/Sema/Ownership.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/Twine.h"
using namespace fortran;

// ParseExpression - Expressions are level-5 expresisons optionally involving
// defined binary operators.
//
//   [7.1.2.8] R722:
//     expr :=
//         [ expr defined-binary-op ] level-5-expr
//
//   [7.1.2.8] R723:
//     defined-binary-op :=
//         . letter [ letter ] ... .
Parser::ExprResult Parser::ParseExpression() {
  ExprResult LHS = ParseLevel5Expr();
  if (LHS.isInvalid()) return ExprResult();

  if (Tok.isNot(tok::defined_operator))
    return LHS;

  llvm::SMLoc OpLoc = Tok.getLocation();
  IdentifierInfo *II = Tok.getIdentifierInfo();
  Lex();

  ExprResult RHS = ParseLevel5Expr();
  if (RHS.isInvalid()) {
    delete LHS.take();
    return ExprResult();
  }

  return new DefinedOperatorBinaryExpr(OpLoc, LHS, RHS, II);
}

// ParseLevel5Expr - Level-5 expressions are level-4 expressions optionally
// involving the logical operators.
//
//   [7.1.2.7] R717:
//     level-5-expr :=
//         [ level-5-expr equiv-op ] equiv-operand
//   [7.1.2.7] R716:
//     equiv-operand :=
//         [ equiv-operand or-op ] or-operand
//   [7.1.2.7] R715:
//     or-operand :=
//         [ or-operand and-op ] and-operand
//   [7.1.2.7] R714:
//     and-operand :=
//         [ not-op ] level-4-expr
//         
//   [7.1.2.7] R718:
//     not-op :=
//         .NOT.
//   [7.1.2.7] R719:
//     and-op :=
//         .AND.
//   [7.1.2.7] R720:
//     or-op :=
//         .OR.
//   [7.1.2.7] R721:
//     equiv-op :=
//         .EQV.
//       | .NEQV.
Parser::ExprResult Parser::ParseAndOperand() {
  llvm::SMLoc NotLoc = Tok.getLocation();
  bool Negate = EatIfPresent(tok::kw_NOT);

  ExprResult E = ParseLevel4Expr();
  if (E.isInvalid()) return ExprResult();

  if (Negate)
    E = new UnaryExpr(NotLoc, UnaryExpr::Not, E);
  return E;
}
Parser::ExprResult Parser::ParseOrOperand() {
  ExprResult E = ParseAndOperand();
  if (E.isInvalid()) return ExprResult();

  while (Tok.getKind() == tok::kw_AND) {
    llvm::SMLoc OpLoc = Tok.getLocation();
    Lex();
    E = new BinaryExpr(OpLoc, BinaryExpr::And, E, ParseAndOperand());
  }

  return E;
}
Parser::ExprResult Parser::ParseEquivOperand() {
  ExprResult E = ParseOrOperand();
  if (E.isInvalid()) return ExprResult();

  while (Tok.getKind() == tok::kw_OR) {
    llvm::SMLoc OpLoc = Tok.getLocation();
    Lex();
    E = new BinaryExpr(OpLoc, BinaryExpr::Or, E, ParseOrOperand());
  }

  return E;
}
Parser::ExprResult Parser::ParseLevel5Expr() {
  ExprResult E = ParseEquivOperand();
  if (E.isInvalid()) return ExprResult();

  while (true) {
    llvm::SMLoc OpLoc = Tok.getLocation();
    switch (Tok.getKind()) {
    default:
      return E;
    case tok::kw_EQV:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::Eqv, E, ParseEquivOperand());
      break;
    case tok::kw_NEQV:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::Neqv, E, ParseEquivOperand());
      break;
    }
  }
}

// ParseLevel4Expr - Level-4 expressions are level-3 expressions optionally
// involving the relational operators.
//
//   [7.1.2.6] R712:
//     level-4-expr :=
//         [ level-3-expr rel-op ] level-3-expr
//   [7.1.2.6] R713:
//     rel-op :=
//         .EQ.
//      |  .NE.
//      |  .LT.
//      |  .LE.
//      |  .GT.
//      |  .GE.
//      |  ==
//      |  /=
//      |  <
//      |  <=
//      |  >
//      |  >=
Parser::ExprResult Parser::ParseLevel4Expr() {
  ExprResult E = ParseLevel3Expr();
  if (E.isInvalid()) return ExprResult();

  while (true) {
    llvm::SMLoc OpLoc = Tok.getLocation();
    switch (Tok.getKind()) {
    default:
      return E;
    case tok::kw_EQ: case tok::equalequal:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::Equal, E, ParseLevel3Expr());
      break;
    case tok::kw_NE: case tok::slashequal:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::NotEqual, E, ParseLevel3Expr());
      break;
    case tok::kw_LT: case tok::less:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::LessThan, E, ParseLevel3Expr());
      break;
    case tok::kw_LE: case tok::lessequal:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::LessThanEqual, E, ParseLevel3Expr());
      break;
    case tok::kw_GT: case tok::greater:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::GreaterThan, E, ParseLevel3Expr());
      break;
    case tok::kw_GE: case tok::greaterequal:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::GreaterThanEqual, E,
                         ParseLevel3Expr());
      break;
    }
  }
}

// ParseLevel3Expr - Level-3 expressions are level-2 expressions optionally
// involving the character operator concat-op.
//
//   [7.1.2.5] R710:
//     level-3-expr :=
//         [ level-3-expr concat-op ] level-2-expr
//   [7.1.2.5] R711:
//     concat-op :=
//         //
Parser::ExprResult Parser::ParseLevel3Expr() {
  ExprResult E = ParseLevel2Expr();
  if (E.isInvalid()) return ExprResult();

  while (Tok.getKind() == tok::slashslash) {
    llvm::SMLoc OpLoc = Tok.getLocation();
    Lex();
    E = new BinaryExpr(OpLoc, BinaryExpr::Concat, E, ParseLevel2Expr());
  }
  
  return E;
}

// ParseLevel2Expr - Level-2 expressions are level-1 expressions optionally
// involving the numeric operators power-op, mult-op, and add-op.
//
//   [7.1.2.4] R706:
//     level-2-expr :=
//         [ [ level-2-expr ] add-op ] add-operand
//   [7.1.2.4] R705:
//     add-operand :=
//         [ add-operand mult-op ] mult-operand
//   [7.1.2.4] R704:
//     mult-operand :=
//         level-1-expr [ power-op mult-operand ]
//   [7.1.2.4] R707:
//     power-op :=
//         **
//   [7.1.2.4] R708:
//     mult-op :=
//         *
//      |  /
//   [7.1.2.4] R709:
//     add-op :=
//         +
//      |  -
Parser::ExprResult Parser::ParseMultOperand() {
  ExprResult E = ParseLevel1Expr();
  if (E.isInvalid()) return ExprResult();

  if (Tok.getKind() == tok::starstar) {
    llvm::SMLoc OpLoc = Tok.getLocation();
    Lex();
    E = new BinaryExpr(OpLoc, BinaryExpr::Power, E, ParseMultOperand());
  }

  return E;
}
Parser::ExprResult Parser::ParseAddOperand() {
  ExprResult E = ParseMultOperand();
  if (E.isInvalid()) return ExprResult();

  while (true) {
    llvm::SMLoc OpLoc = Tok.getLocation();
    switch (Tok.getKind()) {
    default:
      return E;
    case tok::star:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::Multiply, E, ParseMultOperand());
      break;
    case tok::slash:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::Divide, E, ParseMultOperand());
      break;
    }
  }
}
Parser::ExprResult Parser::ParseLevel2Expr() {
  ExprResult E;
  llvm::SMLoc OpLoc = Tok.getLocation();
  tok::TokenKind Kind = Tok.getKind();

  if (Kind == tok::plus || Kind == tok::minus) {
    Lex(); // Eat operand.

    E = ParseAddOperand();
    if (E.isInvalid()) return ExprResult();

    if (Kind == tok::minus)
      E = new UnaryExpr(OpLoc, UnaryExpr::Minus, E);
    else
      E = new UnaryExpr(OpLoc, UnaryExpr::Plus, E);
  } else {
    E = ParseAddOperand();
    if (E.isInvalid()) return ExprResult();
  }

  while (true) {
    OpLoc = Tok.getLocation();
    switch (Tok.getKind()) {
    default:
      return E;
    case tok::plus:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::Plus, E, ParseAddOperand());
      break;
    case tok::minus:
      Lex();
      E = new BinaryExpr(OpLoc, BinaryExpr::Minus, E, ParseAddOperand());
      break;
    }
  }
}

// ParseLevel1Expr - Level-1 expressions are primaries optionally operated on by
// defined unary operators.
//
//   [7.1.2.3] R702:
//     level-1-expr :=
//         [ defined-unary-op ] primary
//   [7.1.2.3] R703:
//     defined-unary-op :=
//         . letter [ letter ] ... .
Parser::ExprResult Parser::ParseLevel1Expr() {
  llvm::SMLoc OpLoc = Tok.getLocation();
  IdentifierInfo *II = 0;
  if (Tok.is(tok::defined_operator)) {
    II = Tok.getIdentifierInfo();
    Lex();
  }

  ExprResult E = ParsePrimaryExpr();
  if (E.isInvalid()) return ExprResult();

  if (II)
    E = new DefinedOperatorUnaryExpr(OpLoc, E, II);

  return E;
}

// ParsePrimaryExpr - Parse a primary expression.
//
//   [7.1.2.2] R701:
//     primary :=
//         constant
//      |  designator
//      |  array-constructor
//      |  structure-constructor
//      |  function-reference
//      |  type-param-inquiry
//      |  type-param-name
//      |  ( expr )
Parser::ExprResult Parser::ParsePrimaryExpr() {
  ExprResult E;
  llvm::SMLoc Loc = Tok.getLocation();

  // FIXME: Add rest of the primary expressions.
  switch (Tok.getKind()) {
  default:
    Diag.ReportError(Loc, "unknown unary expression");
    break;
  case tok::l_paren: {
    Lex();
    E = ParseExpression();
    if (Tok.isNot(tok::r_paren)) {
      delete E.take();
      Diag.ReportError(Tok.getLocation(),
                       "expected ')' in expression");
      return ExprResult();
    }
    Lex();
    break;
  }
  case tok::numeric_constant:
    E = new ConstantExpr(Loc, llvm::StringRef(Tok.getLiteralData(),
                                              Tok.getLength()));
    Lex();
    break;
  case tok::identifier: {
    const VarDecl *VD = Context.getVarDecl(Tok.getIdentifierInfo());
    if (!VD) {
      Diag.ReportError(Loc, "unknown identifier");
      return ExprResult();
    }
    E = new VarExpr(Loc, VD);
    Lex();
    break;
  }
  case tok::minus:
    Lex();
    E = Parser::ParsePrimaryExpr();
    if (E.isInvalid()) return ExprResult();
    E = new UnaryExpr(Loc, UnaryExpr::Minus, E);
    break;
  case tok::plus:
    Lex();
    E = Parser::ParsePrimaryExpr();
    if (E.isInvalid()) return ExprResult();
    E = new UnaryExpr(Loc, UnaryExpr::Plus, E);
    break;
  }

  return E;
}
