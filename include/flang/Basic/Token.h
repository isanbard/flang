//===--- Token.h - Token interface ------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Token interface.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FORTRAN_TOKEN_H__
#define LLVM_FORTRAN_TOKEN_H__

#include "flang/Basic/TokenKinds.h"
#include "llvm/Support/SMLoc.h"
#include <cassert>
#include <cstdlib>

namespace llvm {
  class Twine;
} // end llvm namespace

namespace fortran {

class IdentifierInfo;

/// Token - This structure provides full information about a lexed token.  It is
/// not intended to be space efficient, it is intended to return as much
/// information as possible about each returned token.  This is expected to be
/// compressed into a smaller form if memory footprint is important.
class Token {
  /// The location of the token.
  llvm::SMLoc Loc;

  // Conceptually these next two fields could be in a union. However, this
  // causes gcc 4.2 to pessimize LexTokenInternal, a very performance critical
  // routine. Keeping as separate members with casts until a more beautiful fix
  // presents itself.

  /// UintData - This holds either the length of the token text, when
  /// a normal token, or the end of the SourceRange when an annotation
  /// token.
  unsigned UintData;

  /// PtrData - This is a union of three different pointer types, which depends
  /// on what type of token this is:
  ///  Identifiers, keywords, etc:
  ///    This is an IdentifierInfo*, which contains the uniqued identifier
  ///    spelling.
  ///  Literals:  isLiteral() returns true.
  ///    This is a pointer to the start of the token in a text buffer, which
  ///    may be dirty (span more than one line).
  ///  Other:
  ///    This is null.
  void *PtrData;

  /// Kind - The actual flavor of token this is.
  unsigned Kind : 8;  // DON'T make Kind a 'tok::TokenKind';
                      // MSVC will treat it as a signed char and
                      // TokenKinds > 127 won't be handled correctly.

  /// Flags - Bits we track about this token, members of the TokenFlags enum.
  unsigned Flags : 8;
public:

  // Various flags set per token:
  enum TokenFlags {
    StartOfStatement = 0x01,  // At start of statement or only after whitespace
    NeedsCleaning    = 0x02   // Contained a continuation
  };

  tok::TokenKind getKind() const { return (tok::TokenKind)Kind; }
  void setKind(tok::TokenKind K) { Kind = K; }

  /// is/isNot - Predicates to check if this token is a specific kind, as in
  /// "if (Tok.is(tok::l_brace)) {...}".
  bool is(tok::TokenKind K) const { return Kind == (unsigned) K; }
  bool isNot(tok::TokenKind K) const { return Kind != (unsigned) K; }

  /// isLiteral - Return true if this is a "literal", like a numeric constant,
  /// string, etc.
  bool isLiteral() const {
    return is(tok::numeric_constant)    || is(tok::statement_label)
      || is(tok::char_literal_constant) || is(tok::binary_boz_constant)
      || is(tok::octal_boz_constant)    || is(tok::hex_boz_constant)
      || is(tok::defined_operator)      || is(tok::comment);
  }

  /// getLocation - Return a source location identifier for the specified offset
  /// in the current file.
  llvm::SMLoc getLocation() const { return Loc; }
  unsigned getLength() const { return UintData; }

  void setLocation(llvm::SMLoc L) { Loc = L; }
  void setLength(unsigned Len) { UintData = Len; }

  const char *getName() const {
    return tok::getTokenName((tok::TokenKind)Kind);
  }

  /// startToken - Reset all flags to cleared.
  void startToken() {
    Kind = tok::unknown;
    Flags = 0;
    PtrData = 0;
    Loc = llvm::SMLoc();
  }

  IdentifierInfo *getIdentifierInfo() const {
    if (isLiteral()) return 0;
    return (IdentifierInfo*)PtrData;
  }
  void setIdentifierInfo(IdentifierInfo *II) {
    PtrData = (void*)II;
  }

  /// getLiteralData - For a literal token (numeric constant, string, etc), this
  /// returns a pointer to the start of it in the text buffer if known, null
  /// otherwise.
  const char *getLiteralData() const {
    assert(isLiteral() && "Cannot get literal data of non-literal");
    return reinterpret_cast<const char*>(PtrData);
  }
  void setLiteralData(const char *Ptr) {
    assert(isLiteral() && "Cannot set literal data of non-literal");
    PtrData = (void*)Ptr;
  }

  /// setFlag - Set the specified flag.
  void setFlag(TokenFlags Flag) {
    Flags |= Flag;
  }

  /// clearFlag - Unset the specified flag.
  void clearFlag(TokenFlags Flag) {
    Flags &= ~Flag;
  }

  /// getFlags - Return the internal represtation of the flags. Only intended
  /// for low-level operations such as writing tokens to disk.
  unsigned getFlags() const { return Flags; }

  /// setFlagValue - Set a flag to either true or false.
  void setFlagValue(TokenFlags Flag, bool Val) {
    if (Val)
      setFlag(Flag);
    else
      clearFlag(Flag);
  }

  /// CleanCharContext - Return the string from a character context that was
  /// continued over many lines.
  llvm::Twine CleanCharContext();

  /// isAtStartOfStatement - Return true if this token is at the start of a
  /// statement.
  bool isAtStartOfStatement() const {
    return (Flags & StartOfStatement) ? true : false;
  }

  /// needsCleaning - Return true if this token has a continuation.
  bool needsCleaning() const {
    return (Flags & NeedsCleaning) ? true : false;
  }
};

} // end namespace fortran

#endif
