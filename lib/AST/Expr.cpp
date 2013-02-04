//===--- Expr.cpp - Fortran Expressions -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/Expr.h"
#include "flang/AST/ASTContext.h"
#include "flang/AST/Decl.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
using namespace flang;

void APNumericStorage::setIntValue(ASTContext &C, const APInt &Val) {
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
  : ConstantExpr(IntegerConstant, C.IntegerTy,  Loc) {
  llvm::APSInt Val(64);
  Data.getAsInteger(10, Val);
  Num.setValue(C, Val);
}

IntegerConstantExpr *IntegerConstantExpr::Create(ASTContext &C, SMLoc Loc,
                                                 StringRef Data) {
  return new (C) IntegerConstantExpr(C, Loc, Data);
}

RealConstantExpr::RealConstantExpr(ASTContext &C, SMLoc Loc, StringRef Data)
  : ConstantExpr(RealConstant, C.RealTy /*FIXME: Double?*/, Loc) {
  // FIXME: IEEEdouble?
  APFloat Val(APFloat::IEEEsingle, Data);
  Num.setValue(C, Val);
}

RealConstantExpr *RealConstantExpr::Create(ASTContext &C, SMLoc Loc,
                                           StringRef Data) {
  return new (C) RealConstantExpr(C, Loc, Data);
}

CharacterConstantExpr::CharacterConstantExpr(ASTContext &C, SMLoc Loc,
                                             StringRef data)
  : ConstantExpr(CharacterConstant, C.CharacterTy, Loc) {
  // TODO: A 'kind' on a character literal constant.
  Data = new (C) char[data.size() + 1];
  std::strncpy(Data, data.data(), data.size());
  Data[data.size()] = '\0';
}

CharacterConstantExpr *CharacterConstantExpr::Create(ASTContext &C, SMLoc Loc,
                                                     StringRef Data) {
  return new (C) CharacterConstantExpr(C, Loc, Data);
}

BOZConstantExpr::BOZConstantExpr(ASTContext &C, SMLoc Loc, StringRef Data)
  : ConstantExpr(BOZConstant, C.IntegerTy, Loc) {
  unsigned Radix = 0;
  switch (Data[0]) {
  case 'B':
    Kind = Binary;
    Radix = 2;
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

  size_t LastQuote = Data.rfind(Data[1]);
  assert(LastQuote == StringRef::npos && "Invalid BOZ constant!");
  llvm::StringRef NumStr = Data.slice(2, LastQuote);
  APInt Val;
  NumStr.getAsInteger(Radix, Val);
  Num.setValue(C, Val);
}

BOZConstantExpr *BOZConstantExpr::Create(ASTContext &C, SMLoc Loc,
                                         StringRef Data) {
  return new (C) BOZConstantExpr(C, Loc, Data);
}

LogicalConstantExpr::LogicalConstantExpr(ASTContext &C, SMLoc Loc,
                                         StringRef Data)
  : ConstantExpr(LogicalConstant, C.LogicalTy, Loc) {
  Val = (Data.compare_lower(".TRUE.") == 0);
}

LogicalConstantExpr *LogicalConstantExpr::Create(ASTContext &C, SMLoc Loc,
                                                 StringRef Data) {
  return new (C) LogicalConstantExpr(C, Loc, Data);
}

VarExpr::VarExpr(llvm::SMLoc Loc, const VarDecl *Var)
  : DesignatorExpr(Loc, Var->getType(), DesignatorExpr::ObjectName),
    Variable(Var) {}

VarExpr *VarExpr::Create(ASTContext &C, SMLoc Loc, const VarDecl *VD) {
  return new (C) VarExpr(Loc, VD);
}

UnaryExpr *UnaryExpr::Create(ASTContext &C, SMLoc loc, Operator op,
                             ExprResult e) {
  return new (C) UnaryExpr(Expr::Unary,
                           (op != Not) ? e.get()->getType() : C.LogicalTy,
                           loc, op, e);
}

DefinedOperatorUnaryExpr::DefinedOperatorUnaryExpr(SMLoc loc, ExprResult e,
                                                   IdentifierInfo *ii)
  : UnaryExpr(Expr::DefinedUnaryOperator, e.get()->getType(), loc, Defined, e),
    II(ii) {}

DefinedOperatorUnaryExpr *DefinedOperatorUnaryExpr::Create(ASTContext &C,
                                                           SMLoc loc,
                                                           ExprResult e,
                                                           IdentifierInfo *ii) {
  return new (C) DefinedOperatorUnaryExpr(loc, e, ii);
}

BinaryExpr *BinaryExpr::Create(ASTContext &C, SMLoc loc, Operator op,
                               ExprResult lhs, ExprResult rhs) {
  QualType Ty;

  switch (op) {
  default: {
    // FIXME: Combine two types.
    Ty = lhs.get()->getType();
    break;
  }
  case Eqv: case Neqv: case Or: case And:
  case Equal: case NotEqual: case LessThan: case LessThanEqual:
  case GreaterThan: case GreaterThanEqual:
    Ty = C.LogicalTy;
    break;
  case Concat:
    Ty = C.CharacterTy;
    break;
  }

  return new (C) BinaryExpr(Expr::Binary, Ty, loc, op, lhs, rhs);
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
  if (Kind)
    O << '_' << Kind; 
}

void IntegerConstantExpr::print(llvm::raw_ostream &O) {
  O << Num.getValue();
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
// Array Specification
//===----------------------------------------------------------------------===//

ArraySpec::ArraySpec(ArraySpecKind K)
  : Kind(K) {}

ExplicitShapeSpec::ExplicitShapeSpec(ExprResult UB)
  : ArraySpec(k_ExplicitShape), LowerBound(), UpperBound(UB) {}
ExplicitShapeSpec::ExplicitShapeSpec(ExprResult LB, ExprResult UB)
  : ArraySpec(k_ExplicitShape), LowerBound(LB), UpperBound(UB) {}

ExplicitShapeSpec *ExplicitShapeSpec::Create(ASTContext &C, ExprResult UB) {
  return new (C) ExplicitShapeSpec(UB);
}

ExplicitShapeSpec *ExplicitShapeSpec::Create(ASTContext &C,
                                             ExprResult LB, ExprResult UB) {
  return new (C) ExplicitShapeSpec(LB, UB);
}

AssumedShapeSpec::AssumedShapeSpec()
  : ArraySpec(k_AssumedShape), LowerBound() {}
AssumedShapeSpec::AssumedShapeSpec(ExprResult LB)
  : ArraySpec(k_AssumedShape), LowerBound(LB) {}

AssumedShapeSpec *AssumedShapeSpec::Create(ASTContext &C) {
  return new (C) AssumedShapeSpec();
}

AssumedShapeSpec *AssumedShapeSpec::Create(ASTContext &C, ExprResult LB) {
  return new (C) AssumedShapeSpec(LB);
}

DeferredShapeSpec::DeferredShapeSpec()
  : ArraySpec(k_DeferredShape) {}

DeferredShapeSpec *DeferredShapeSpec::Create(ASTContext &C) {
  return new (C) DeferredShapeSpec();
}

ImpliedShapeSpec::ImpliedShapeSpec()
  : ArraySpec(k_ImpliedShape), LowerBound() {}
ImpliedShapeSpec::ImpliedShapeSpec(ExprResult LB)
  : ArraySpec(k_ImpliedShape), LowerBound(LB) {}

ImpliedShapeSpec *ImpliedShapeSpec::Create(ASTContext &C) {
  return new (C) ImpliedShapeSpec();
}

ImpliedShapeSpec *ImpliedShapeSpec::Create(ASTContext &C, ExprResult LB) {
  return new (C) ImpliedShapeSpec(LB);
}
