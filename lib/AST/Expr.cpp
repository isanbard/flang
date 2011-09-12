//===--- Expr.cpp - Fortran Expressions -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/ASTContext.h"
#include "flang/AST/Decl.h"
#include "flang/AST/Expr.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringRef.h"
using namespace flang;

IntegerConstantExpr::IntegerConstantExpr(llvm::SMLoc Loc, llvm::StringRef Data)
  : ConstantExpr(IntegerConstant, Loc) {
  std::pair<StringRef, StringRef> StrPair = Data.split('_');
  if (!StrPair.second.empty())
    setKindSelector(StrPair.second);
  Val = APInt(APInt::getBitsNeeded(StrPair.first, 10), StrPair.first, 10);
}

IntegerConstantExpr *IntegerConstantExpr::Create(ASTContext &C, llvm::SMLoc Loc,
                                                 llvm::StringRef Data) {
  return new (C) IntegerConstantExpr(Loc, Data);
}

RealConstantExpr::RealConstantExpr(llvm::SMLoc Loc, llvm::StringRef Data)
  : ConstantExpr(RealConstant, Loc), Val(APFloat::IEEEsingle) {
  std::pair<StringRef, StringRef> StrPair = Data.split('_');
  if (!StrPair.second.empty())
    setKindSelector(StrPair.second);
  // FIXME: IEEEdouble?
  Val = APFloat(APFloat::IEEEsingle, StrPair.first);
}

RealConstantExpr *RealConstantExpr::Create(ASTContext &C, llvm::SMLoc Loc,
                                                 llvm::StringRef Data) {
  return new (C) RealConstantExpr(Loc, Data);
}

BOZConstantExpr::BOZConstantExpr(llvm::SMLoc Loc, llvm::StringRef Data)
  : ConstantExpr(BOZConstant, Loc) {
  std::pair<StringRef, StringRef> StrPair = Data.split('_');
  if (!StrPair.second.empty())
    setKindSelector(StrPair.second);

  unsigned Radix = 0;
  switch (StrPair.first[0]) {
  case 'B':
    Kind = Binary;
    Radix = 1;
    break;
  case 'O':
    Kind = Octal;
    Radix = 8;
    break;
  case 'Z': case 'X':
    Kind = Hexadecimal;
    Radix = 16;
    break;
  }

  size_t LastQuote = StrPair.first.rfind(StrPair.first[1]);
  assert(LastQuote == StringRef::npos && "Invalid BOZ constant!");
  llvm::StringRef Num = StrPair.first.slice(2, LastQuote);
  Value = APInt(APInt::getBitsNeeded(Num, Radix), Num, Radix);
}

BOZConstantExpr *BOZConstantExpr::Create(ASTContext &C, llvm::SMLoc Loc,
                                         llvm::StringRef Data) {
  return new (C) BOZConstantExpr(Loc, Data);
}

LogicalConstantExpr::LogicalConstantExpr(llvm::SMLoc Loc, llvm::StringRef Data)
  : ConstantExpr(LogicalConstant, Loc) {
  std::pair<StringRef, StringRef> StrPair = Data.split('_');
  if (!StrPair.second.empty())
    setKindSelector(StrPair.second);

  Kind = (StrPair.first.compare_upper(".TRUE.") == 0);
}

LogicalConstantExpr *LogicalConstantExpr::Create(ASTContext &C, llvm::SMLoc Loc,
                                                 llvm::StringRef Data) {
  return new (C) LogicalConstantExpr(Loc, Data);
}

//===----------------------------------------------------------------------===//
// Expression D'tors
//===----------------------------------------------------------------------===//

Expr::~Expr() {}

DesignatorExpr::~DesignatorExpr() {}

UnaryExpr::~UnaryExpr() {
  delete E.take();
}

BinaryExpr::~BinaryExpr() {
  delete LHS.take();
  delete RHS.take();
}

//===----------------------------------------------------------------------===//
// Expression Print Statements
//===----------------------------------------------------------------------===//

void Expr::dump() {
  this->print(llvm::outs());
}

void Expr::print(llvm::raw_ostream &O) {
}

void DesignatorExpr::print(llvm::raw_ostream &O) {
}

void UnaryExpr::print(llvm::raw_ostream &O) {
  O << '(';
  const char *op = 0;
  switch (Op) {
  default: break;
  case Not:   op = ".NOT."; break;
  case Plus:  op = "+";     break;
  case Minus: op = "-";     break;
  }
  O << op;
  E.get()->print(O);
  O << ')';
}

void DefinedOperatorUnaryExpr::print(llvm::raw_ostream &O) {
  O << '(' << II->getName();
  E.get()->print(O);
  O << ')';
}

void ConstantExpr::print(llvm::raw_ostream &O) {
  O << Data;
}

void VarExpr::print(llvm::raw_ostream &O) {
  O << *Variable;
}

void BinaryExpr::print(llvm::raw_ostream &O) {
  O << '(';
  LHS.get()->print(O);
  const char *op = 0;
  switch (Op) {
  default: break;
  case Eqv:              op = ".EQV.";  break;
  case Neqv:             op = ".NEQV."; break;
  case Or:               op = ".OR.";   break;
  case And:              op = ".AND.";  break;
  case Equal:            op = "==";     break;
  case NotEqual:         op = "/=";     break;
  case LessThan:         op = "<";      break;
  case LessThanEqual:    op = "<=";     break;
  case GreaterThan:      op = ">";      break;
  case GreaterThanEqual: op = ">=";     break;
  case Concat:           op = "//";     break;
  case Plus:             op = "+";      break;
  case Minus:            op = "-";      break;
  case Multiply:         op = "*";      break;
  case Divide:           op = "/";      break;
  case Power:            op = "**";     break;
  }
  O << op;
  RHS.get()->print(O);
  O << ')';
}

void DefinedOperatorBinaryExpr::print(llvm::raw_ostream &O) {
  O << '(';
  LHS.get()->print(O);
  II->getName();
  RHS.get()->print(O);
  O << ')';
}

//===----------------------------------------------------------------------===//
// Subscript Methods
//===----------------------------------------------------------------------===//

Subscript::~Subscript() {
}

Subscript *Subscript::create(Expr *E) {
  return new Subscript(Subscript::Normal, E);
}

Subscript *Subscript::createEmptyRange() {
  return new Subscript(Subscript::EmptyRange);
}

Subscript *Subscript::createSplat() {
  return new Subscript(Subscript::Splat);
}
