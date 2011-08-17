//===--- TokenKinds.cpp - Token Kinds Support -----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the TokenKind enum and support functions.
//
//===----------------------------------------------------------------------===//

#include "flang/Basic/TokenKinds.h"
#include <cassert>

using namespace fortran;

static char const * const TokNames[] = {
#define TOK(X)       #X,
#define KEYWORD(X,Y) #X,
#define BUILTIN(X,Y) #X,
#include "flang/Basic/TokenKinds.def"
  0
};

const char *tok::getTokenName(enum TokenKind Kind) {
  assert(Kind < tok::NUM_TOKENS && "Invalid token kind!");
  return TokNames[Kind];
}

const char *tok::getTokenSimpleSpelling(enum TokenKind Kind) {
  switch (Kind) {
  case tok::equal:               return "=";
  case tok::equalequal:          return "==";
  case tok::plus:                return "+";
  case tok::minus:               return "-";
  case tok::star:                return "*";
  case tok::starstar:            return "**";
  case tok::slash:               return "/";
  case tok::slashequal:          return "/=";
  case tok::backslash:           return "\\";
  case tok::l_paren:             return "(";
  case tok::r_paren:             return ")";
  case tok::l_square:            return "[";
  case tok::r_square:            return "]";
  case tok::l_brace:             return "{";
  case tok::r_brace:             return "}";
  case tok::comma:               return ",";
  case tok::period:              return ".";
  case tok::colon:               return ":";
  case tok::coloncolon:          return "::";
  case tok::semicolon:           return ";";
  case tok::exclaim:             return "!";
  case tok::percent:             return "%";
  case tok::ampersand:           return "&";
  case tok::tilde:               return "~";
  case tok::less:                return "<";
  case tok::lessequal:           return "<=";
  case tok::greater:             return ">";
  case tok::greaterequal:        return ">=";
  case tok::question:            return "?";
  case tok::backtick:            return "`";
  case tok::caret:               return "^";
  case tok::pipe:                return "|";
  case tok::dollar:              return "$";
  case tok::hash:                return "#";
  case tok::at:                  return "@";
  default: break;
  }

  return 0;
}
