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

void APNumericStorage::setIntValue(ASTContext &C, const llvm::APInt &Val) {
  if (hasAllocation())
    C.Deallocate(pVal);

  BitWidth = Val.getBitWidth();
  unsigned NumWords = Val.getNumWords();
  const uint64_t* Words = Val.getRawData();
  if (NumWords > 1) {
    pVal = new (C) uint64_t[NumWords];
    std::copy(Words, Words + NumWords, pVal);
  } else if (NumWords == 1)
    VAL = Words[0];
  else
    VAL = 0;
}

IntegerConstantExpr::IntegerConstantExpr(ASTContext &C, SMLoc Loc,
                                         StringRef Data)
  : ConstantExpr(IntegerConstant, Loc) {
  std::pair<StringRef, StringRef> StrPair = Data.split('_');
  if (!StrPair.second.empty())
    setKindSelector(StrPair.second);
  APInt Val(APInt::getBitsNeeded(StrPair.first, 10), StrPair.first, 10);
  Num.setValue(C, Val);
}

IntegerConstantExpr *IntegerConstantExpr::Create(ASTContext &C, llvm::SMLoc Loc,
                                                 llvm::StringRef Data) {
  return new (C) IntegerConstantExpr(C, Loc, Data);
}

RealConstantExpr::RealConstantExpr(ASTContext &C, SMLoc Loc, StringRef Data)
  : ConstantExpr(RealConstant, Loc) {
  std::pair<StringRef, StringRef> StrPair = Data.split('_');
  if (!StrPair.second.empty())
    setKindSelector(StrPair.second);
  // FIXME: IEEEdouble?
  APFloat Val(APFloat::IEEEsingle, StrPair.first);
  Num.setValue(C, Val);
}

RealConstantExpr *RealConstantExpr::Create(ASTContext &C, SMLoc Loc,
                                           StringRef Data) {
  return new (C) RealConstantExpr(C, Loc, Data);
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
  Val = APInt(APInt::getBitsNeeded(Num, Radix), Num, Radix);
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

  Val = (StrPair.first.compare_upper(".TRUE.") == 0);
}

LogicalConstantExpr *LogicalConstantExpr::Create(ASTContext &C, llvm::SMLoc Loc,
                                                 llvm::StringRef Data) {
  return new (C) LogicalConstantExpr(Loc, Data);
}

UnaryExpr *UnaryExpr::Create(ASTContext &C, SMLoc loc, Operator op,
                             ExprResult e) {
  return new (C) UnaryExpr(Expr::Unary, loc, op, e);
}

DefinedOperatorUnaryExpr *DefinedOperatorUnaryExpr::Create(ASTContext &C,
                                                           SMLoc loc,
                                                           ExprResult e,
                                                           IdentifierInfo *ii) {
  return new (C) DefinedOperatorUnaryExpr(loc, e, ii);
}

BinaryExpr *BinaryExpr::Create(ASTContext &C, SMLoc loc, Operator op,
                               ExprResult lhs, ExprResult rhs) {
  return new (C) BinaryExpr(Expr::Binary, loc, op, lhs, rhs);
}

DefinedOperatorBinaryExpr *
DefinedOperatorBinaryExpr::Create(ASTContext &C, SMLoc loc, ExprResult lhs,
                                  ExprResult rhs, IdentifierInfo *ii) {
  return new (C) DefinedOperatorBinaryExpr(loc, lhs, rhs, ii);
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

Subscript *Subscript::create(Expr *E) {
  return new Subscript(Subscript::Normal, E);
}

Subscript *Subscript::createEmptyRange() {
  return new Subscript(Subscript::EmptyRange);
}

Subscript *Subscript::createSplat() {
  return new Subscript(Subscript::Splat);
}
