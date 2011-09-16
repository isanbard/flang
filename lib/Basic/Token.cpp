//===--- Token.cpp - Token implementation ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Token interface.
//
//===----------------------------------------------------------------------===//

#include "flang/Basic/Token.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "flang/Basic/LLVM.h"
using namespace flang;

static inline bool isHorizontalWhitespace(char C) {
  return C == ' ' || C == '\t' || C == '\f' || C == '\v';
}

/// CleanLiteral - Return the literal cleaned up of any line continuations.
std::string Token::CleanLiteral(SmallVectorImpl<StringRef> &Spelling) const {
  if (!needsCleaning())
    return Spelling[0].str();

  std::string Name;
  Name.reserve(256);
  for (llvm::SmallVectorImpl<StringRef>::const_iterator
         I = Spelling.begin(), E = Spelling.end(); I != E; ++I)
    Name += *I;

  return Name;
}

/// CleanCharContext - Clean up a character context which is "dirty" (has
/// continuations in it).
llvm::Twine Token::CleanCharContext() {
  assert(is(tok::char_literal_constant) && needsCleaning() &&
         "Trying to clean a pristene character context!");
  llvm::Twine CharContext;
  const char *CurPtr = getLiteralData();
  const char *Start = Start;

  while (true) {
    while (*CurPtr != '&') {
      if (*CurPtr != '\'')
        ++CurPtr;
      else if (CurPtr[1] == '\'')
        CurPtr += 2;
      else
        break;
    }

    if (*CurPtr == '\'') break;

    const char *Amp = CurPtr++;
    while (isHorizontalWhitespace(*CurPtr))
      ++CurPtr;

    if (*CurPtr != '\n' && *CurPtr != '\r')
      continue;

    CharContext = CharContext +
      llvm::Twine(llvm::StringRef(Start, Amp - Start));

    while (true) {
      while (isHorizontalWhitespace(*CurPtr) ||
             *CurPtr == '\n' || *CurPtr == '\r')
        ++CurPtr;

      if (*CurPtr == '!') {
        while (*CurPtr != '\n' && *CurPtr != '\r')
          ++CurPtr;
      } else {
        break;
      }
    }

    Start = ++CurPtr;
  }

  return CharContext +
    llvm::Twine(llvm::StringRef(CurPtr, Start - CurPtr));
}
