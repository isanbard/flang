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
//   R722:
//     expr :=
//         [ expr defined-binary-op ] level-5-expr
//
//   R723:
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
//   R717:
//     level-5-expr :=
//         [ level-5-expr equiv-op ] equiv-operand
//   R716:
//     equiv-operand :=
//         [ equiv-operand or-op ] or-operand
//   R715:
//     or-operand :=
//         [ or-operand and-op ] and-operand
//   R714:
//     and-operand :=
//         [ not-op ] level-4-expr
//         
//   R718:
//     not-op :=
//         .NOT.
//   R719:
//     and-op :=
//         .AND.
//   R720:
//     or-op :=
//         .OR.
//   R721:
//     equiv-op :=
//         .EQV.
//      or .NEQV.
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
//   R712:
//     level-4-expr :=
//         [ level-3-expr rel-op ] level-3-expr
//   R713:
//     rel-op :=
//         .EQ.
//      or .NE.
//      or .LT.
//      or .LE.
//      or .GT.
//      or .GE.
//      or ==
//      or /=
//      or <
//      or <=
//      or >
//      or >=
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
//   R710:
//     level-3-expr :=
//         [ level-3-expr concat-op ] level-2-expr
//   R711:
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
//   R706:
//     level-2-expr :=
//         [ [ level-2-expr ] add-op ] add-operand
//   R705:
//     add-operand :=
//         [ add-operand mult-op ] mult-operand
//   R704:
//     mult-operand :=
//         level-1-expr [ power-op mult-operand ]
//   R707:
//     power-op :=
//         **
//   R708:
//     mult-op :=
//         *
//      or /
//   R709:
//     add-op :=
//         +
//      or -
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
//   R702:
//     level-1-expr :=
//         [ defined-unary-op ] primary
//   R703:
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
//   R701:
//     primary :=
//         constant
//      or designator
//      or array-constructor
//      or structure-constructor
//      or function-reference
//      or type-param-inquiry
//      or type-param-name
//      or ( expr )
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
  case tok::char_literal_constant:
  case tok::identifier:
    E = Parser::ParseDesignator();
    if (E.isInvalid()) return ExprResult();
    break;
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

/// ParseDesignator - Parse a designator. Return null if current token is not a
/// designator.
///
///   R601:
///     designator :=
///         object-name
///      or array-element
///      or array-section
///      or coindexed-named-object
///      or complex-part-designator
///      or structure-component
///      or substring
ExprResult Parser::ParseDesignator() {
  if (Tok.is(tok::char_literal_constant))
    // Possibly something like: '0123456789'(N:N)
    return ParseSubstring();

  ExprResult E;
  if (Tok.isNot(tok::identifier)) return E;

  // R504:
  //   object-name :=
  //       name
  E = new VarExpr(Tok.getLocation(),
                  Context.getOrCreateVarDecl(Tok.getIdentifierInfo()));
  Lex();

  return E;
}

/// ParseArrayElement - Parse an array element.
/// 
///   R617:
///     array-element :=
///         data-ref
///   R611:
///     data-ref :=
///         part-ref [ % part-ref ] ...
ExprResult Parser::ParseArrayElement() {
  ExprResult E;
  return E;
}

/// ParseArraySection - Parse a array section.
///
///   R618:
///     array-section :=
///         data-ref [ ( substring-range ) ]
///      or complex-part-designator
///   R610:
///     substring-range :=
///         [ scalar-int-expr ] : [ scalar-int-expr ]
ExprResult Parser::ParseArraySection() {
  ExprResult E;
  return E;
}

/// ParseCoindexedNamedObject - Parse a coindexed named object.
///
///   R614:
///     coindexed-named-object :=
///         data-ref
///   C620:
///     The data-ref shall contain exactly one part-re. The part-ref shall
///     contain an image-selector. The part-name shall be the name of a scalar
///     coarray.
ExprResult Parser::ParseCoindexedNamedObject() {
  ExprResult E;
  return E;
}

/// ParseComplexPartDesignator - Parse a complex part designator.
///
///   R615:
///     complex-part-designator :=
///         designator % RE
///      or designator % IM
///   C621:
///     The designator shall be of complex type.
ExprResult Parser::ParseComplexPartDesignator() {
  ExprResult E;
  return E;
}

/// ParseStructureComponent - Parse a structure component.
///
///   R613:
///     structure-component :=
///         data-ref
ExprResult Parser::ParseStructureComponent() {
  ExprResult E;
  return E;
}

/// ParseSubstring - Parse a substring.
///
///   R608:
///     substring :=
///         parent-string ( substring-range )
///   R609:
///     parent-string :=
///         scalar-variable-name
///      or array-element
///      or coindexed-named-object
///      or scalar-structure-component
///      or scalar-constant
///   R610:
///     substring-range :=
///         [ scalar-int-expr ] : [ scalar-int-expr ]
ExprResult Parser::ParseSubstring() {
  ExprResult E;
  return E;
}

/// ParsePartReference - Parse the part reference.
///
///   R612:
///     part-ref :=
///         part-name [ ( section-subscript-list ) ] [ image-selector ]
///   R620:
///     section-subscript :=
///         subscript
///      or subscript-triplet
///      or vector-subscript
///   R619:
///     subscript :=
///         scalar-int-expr
///   R621:
///     subscript-triplet :=
///         [ subscript ] : [ subscript ] [ : stride ]
///   R622:
///     stride :=
///         scalar-int-expr
///   R623:
///     vector-subscript :=
///         int-expr
ExprResult Parser::ParsePartReference() {
  ExprResult E;
  return E;
}
