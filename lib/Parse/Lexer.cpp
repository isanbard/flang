//===-- Lexer.cpp - Fortran Lexer Implementation --------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implementation of the Fortran lexer.
//
//===----------------------------------------------------------------------===//

#include "flang/Parse/Lexer.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/Twine.h"
using namespace flang;

static void InitCharacterInfo();

Lexer::Lexer(llvm::SourceMgr &SM, const LangOptions &features, Diagnostic &D)
  : Diags(D), SrcMgr(SM), Features(features), LineBegin(0), TokStart(0),
    LastTokenWasSemicolon(false), LastTokenWasAmpersand(false) {
  InitCharacterInfo();
}

void Lexer::setBuffer(const llvm::MemoryBuffer *Buf, const char *Ptr) {
  CurBuf = Buf;
  LineBegin = BufPtr = (Ptr ? Ptr : CurBuf->getBufferStart());
  CurPtr = 0;
  TokStart = 0;
  std::memset(LineBuf, 0, sizeof(LineBuf));
}

llvm::SMLoc Lexer::getLoc() const {
  return llvm::SMLoc::getFromPointer(TokStart);
}

//===----------------------------------------------------------------------===//
// Character information.
//===----------------------------------------------------------------------===//

enum {
  CHAR_HORZ_WS  = 0x01,  // ' ', '\t', '\f', '\v'.  Note, no '\0'
  CHAR_VERT_WS  = 0x02,  // '\r', '\n'
  CHAR_LETTER   = 0x04,  // a-z,A-Z
  CHAR_NUMBER   = 0x08,  // 0-9
  CHAR_UNDER    = 0x10,  // _
  CHAR_PERIOD   = 0x20,  // .
  CHAR_HEX      = 0x40   // [a-fA-F]
};

// Statically initialize CharInfo table based on ASCII character set
// Reference: FreeBSD 7.2 /usr/share/misc/ascii
static const unsigned char CharInfo[256] = {
// 0 NUL         1 SOH         2 STX         3 ETX
// 4 EOT         5 ENQ         6 ACK         7 BEL
   0           , 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
// 8 BS          9 HT         10 NL         11 VT
//12 NP         13 CR         14 SO         15 SI
   0           , CHAR_HORZ_WS, CHAR_VERT_WS, CHAR_HORZ_WS,
   CHAR_HORZ_WS, CHAR_VERT_WS, 0           , 0           ,
//16 DLE        17 DC1        18 DC2        19 DC3
//20 DC4        21 NAK        22 SYN        23 ETB
   0           , 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
//24 CAN        25 EM         26 SUB        27 ESC
//28 FS         29 GS         30 RS         31 US
   0           , 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
//32 SP         33  !         34  "         35  #
//36  $         37  %         38  &         39  '
   CHAR_HORZ_WS, 0           , 0           , 0           ,
   0           , 0           , 0           , 0           ,
//40  (         41  )         42  *         43  +
//44  ,         45  -         46  .         47  /
   0           , 0           , 0           , 0           ,
   0           , 0           , CHAR_PERIOD , 0           ,
//48  0         49  1         50  2         51  3
//52  4         53  5         54  6         55  7
   CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER ,
   CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER , CHAR_NUMBER ,
//56  8         57  9         58  :         59  ;
//60  <         61  =         62  >         63  ?
   CHAR_NUMBER , CHAR_NUMBER , 0           , 0           ,
   0           , 0           , 0           , 0           ,
//64  @
   0           ,
//65  A
   (CHAR_HEX | CHAR_LETTER),
//66  B
   (CHAR_HEX | CHAR_LETTER),
//67  C
   (CHAR_HEX | CHAR_LETTER),
//68  D
   (CHAR_HEX | CHAR_LETTER),
//69  E
   (CHAR_HEX | CHAR_LETTER),
//70  F
   (CHAR_HEX | CHAR_LETTER),
//71  G
   CHAR_LETTER ,
//72  H         73  I         74  J         75  K
//76  L         77  M         78  N         79  O
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//80  P         81  Q         82  R         83  S
//84  T         85  U         86  V         87  W
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//88  X         89  Y         90  Z         91  [
//92  \         93  ]         94  ^         95  _
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , 0           ,
   0           , 0           , 0           , CHAR_UNDER  ,
//96  `
   0           ,
//97  a
   (CHAR_HEX | CHAR_LETTER),
//98  b
   (CHAR_HEX | CHAR_LETTER),
//99  c
   (CHAR_HEX | CHAR_LETTER),
//100  d
   (CHAR_HEX | CHAR_LETTER),
//101  e
   (CHAR_HEX | CHAR_LETTER),
//102  f
   (CHAR_HEX | CHAR_LETTER),
//103  g
   CHAR_LETTER ,
//104  h        105  i        106  j        107  k
//108  l        109  m        110  n        111  o
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//112  p        113  q        114  r        115  s
//116  t        117  u        118  v        119  w
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , CHAR_LETTER ,
//120  x        121  y        122  z        123  {
//124  |        125  }        126  ~        127 DEL
   CHAR_LETTER , CHAR_LETTER , CHAR_LETTER , 0           ,
   0           , 0           , 0           , 0
};

static void InitCharacterInfo() {
  static bool isInited = false;
  if (isInited) return;

  // Check the statically-initialized CharInfo table
  assert(CHAR_HORZ_WS == CharInfo[(int)' ']);
  assert(CHAR_HORZ_WS == CharInfo[(int)'\t']);
  assert(CHAR_HORZ_WS == CharInfo[(int)'\f']);
  assert(CHAR_HORZ_WS == CharInfo[(int)'\v']);
  assert(CHAR_VERT_WS == CharInfo[(int)'\n']);
  assert(CHAR_VERT_WS == CharInfo[(int)'\r']);
  assert(CHAR_UNDER   == CharInfo[(int)'_']);
  assert(CHAR_PERIOD  == CharInfo[(int)'.']);

  for (unsigned i = 'a'; i <= 'f'; ++i) {
    assert((CHAR_LETTER|CHAR_HEX) == CharInfo[i]);
    assert((CHAR_LETTER|CHAR_HEX) == CharInfo[i+'A'-'a']);
  }

  for (unsigned i = 'g'; i <= 'z'; ++i) {
    assert(CHAR_LETTER == CharInfo[i]);
    assert(CHAR_LETTER == CharInfo[i+'A'-'a']);
  }

  for (unsigned i = '0'; i <= '9'; ++i)
    assert(CHAR_NUMBER == CharInfo[i]);

  isInited = true;
}

/// isIdentifierBody - Return true if this is the body character of an
/// identifier, which is [a-zA-Z0-9_].
static inline bool isIdentifierBody(unsigned char c) {
  return (CharInfo[c] & (CHAR_LETTER | CHAR_NUMBER | CHAR_UNDER)) ?
    true : false;
}

/// isLetter - Return true if this is a letter, which is [a-zA-Z].
static inline bool isLetter(unsigned char c) {
  return (CharInfo[c] & CHAR_LETTER) ? true : false;
}

/// isHorizontalWhitespace - Return true if this character is horizontal
/// whitespace: ' ', '\t', '\f', '\v'.  Note that this returns false for '\0'.
static inline bool isHorizontalWhitespace(unsigned char c) {
  return (CharInfo[c] & CHAR_HORZ_WS) ? true : false;
}

/// isVerticalWhitespace - Return true if this character is vertical whitespace:
/// '\n', '\r'.  Note that this returns false for '\0'.
static inline bool isVerticalWhitespace(unsigned char c) {
  return (CharInfo[c] & CHAR_VERT_WS) ? true : false;
}

/// isWhitespace - Return true if this character is horizontal or vertical
/// whitespace: ' ', '\t', '\f', '\v', '\n', '\r'.  Note that this returns false
/// for '\0'.
static inline bool isWhitespace(unsigned char c) {
  return (CharInfo[c] & (CHAR_HORZ_WS | CHAR_VERT_WS)) ? true : false;
}

/// isNumberBody - Return true if this is the body character of a number, which
/// is [0-9_.].
static inline bool isNumberBody(unsigned char c) {
  return (CharInfo[c] & (CHAR_NUMBER | CHAR_UNDER | CHAR_PERIOD)) ?
    true : false;
}

/// isBinaryNumberBody - Return true if this is the body character of a binary
/// number, which is [01].
static inline bool isBinaryNumberBody(unsigned char c) {
  return (c == '0' | c == '1') ? true : false;
}

/// isOctalNumberBody - Return true if this is the body character of an octal
/// number, which is [0-7].
static inline bool isOctalNumberBody(unsigned char c) {
  return (c >= '0' & c <= '7') ? true : false;
}

/// isDecimalNumberBody - Return true if this is the body character of a decimal
/// number, which is [0-9].
static inline bool isDecimalNumberBody(unsigned char c) {
  return (CharInfo[c] & CHAR_NUMBER) ? true : false;
}

/// isHexNumberBody - Return true if this is the body character of a hexadecimal
/// number, which is [0-9a-fA-F].
static inline bool isHexNumberBody(unsigned char c) {
  return (CharInfo[c] & (CHAR_NUMBER | CHAR_HEX)) ? true : false;
}

//===----------------------------------------------------------------------===//
// Token Spelling
//===----------------------------------------------------------------------===//

/// getSpelling() - Return the 'spelling' of this token.  The spelling of a
/// token are the characters used to represent the token in the source file.
void Lexer::getSpelling(const Token &Tok,
                        llvm::SmallVectorImpl<llvm::StringRef> &Spelling) const{
  assert((int)Tok.getLength() >= 0 && "Token character range is bogus!");

  const char *TokStart = Tok.isLiteral() ?
    Tok.getLiteralData() : Tok.getLocation().getPointer();
  unsigned TokLen = Tok.getLength();

  // If this token contains nothing interesting, return it directly.
  if (!Tok.needsCleaning())
    return Spelling.push_back(llvm::StringRef(TokStart, TokLen));

  const char *CurPtr = TokStart;
  const char *Start = TokStart;
  unsigned Len = 0;

  while (true) {
    while (Len != TokLen) {
      if (*CurPtr != '&') {
        ++CurPtr, ++Len;
        continue;
      }
      if (Tok.isNot(tok::char_literal_constant))
        break;
      const char *TmpPtr = CurPtr + 1;
      unsigned TmpLen = Len + 1;
      while (TmpLen != TokLen && isHorizontalWhitespace(*TmpPtr))
        ++TmpPtr, ++TmpLen;
      if (*TmpPtr == '\n' || *TmpPtr == '\r')
        break;
      CurPtr = TmpPtr;
      Len = TmpLen;
    }

    Spelling.push_back(llvm::StringRef(Start, CurPtr - Start));

    if (*CurPtr != '&' || Len >= TokLen)
      break;

    Start = ++CurPtr; ++Len;

    if (Len >= TokLen)
      break;

    while (true) {
      // Skip blank lines...
      while (Len != TokLen && isWhitespace(*CurPtr))
        ++CurPtr, ++Len;

      if (*CurPtr != '!')
        break;

      // ...and lines with only comments.
      while (Len != TokLen && *CurPtr != '\n' && *CurPtr != '\r')
        ++CurPtr, ++Len;
    }

    if (*CurPtr != '&' || Len >= TokLen)
      break;

    Start = ++CurPtr; ++Len;
  }
}

//===----------------------------------------------------------------------===//
// Helper methods for lexing.
//===----------------------------------------------------------------------===//

/// FormTokenWithChars - When we lex a token, we have identified a span starting
/// at CurPtr, going to TokEnd that forms the token. This method takes that
/// range and assigns it to the token as its location and size. In addition,
/// since tokens cannot overlap, this also updates CurPtr to be TokEnd.
void Lexer::FormTokenWithChars(Token &Result, tok::TokenKind Kind) {
  uint64_t TokLen = (LineBegin + CurPtr) - TokStart;
  CurKind = Kind;
  Result.setLocation(llvm::SMLoc::getFromPointer(TokStart));
  Result.setLength(TokLen);
  Result.setKind(Kind);
}

/// FormDefinedOperatorTokenWithChars - A special form of FormTokenWithChars. It
/// will see if the defined operator is an intrinsic operator. If so, it will
/// set the token's kind to that value.
void Lexer::FormDefinedOperatorTokenWithChars(Token &Result) {
  unsigned TokLen = (LineBegin + CurPtr) - TokStart;
  assert(TokLen >= 2 && "Malformed defined operator!");

  if (TokLen - 2 > 63)
    // TODO: Emit an error.
    return FormTokenWithChars(Result, tok::unknown);

  llvm::StringRef FullOp(TokStart, TokLen);
  size_t Under = FullOp.find('_');
  llvm::StringRef Op = FullOp;

  if (Under != llvm::StringRef::npos)
    Op = FullOp.substr(0, Under);

  tok::TokenKind Kind = tok::defined_operator;

  if (Op.compare_upper(".TRUE.") == 0 || Op.compare_upper(".FALSE.") == 0)
    Kind = tok::logical_literal_constant;
  else if (Op.compare_upper(".EQ.") == 0)
    Kind = tok::kw_EQ;
  else if (Op.compare_upper(".NE.") == 0)
    Kind = tok::kw_NE;
  else if (Op.compare_upper(".LT.") == 0)
    Kind = tok::kw_LT;
  else if (Op.compare_upper(".LE.") == 0)
    Kind = tok::kw_LE;
  else if (Op.compare_upper(".GT.") == 0)
    Kind = tok::kw_GT;
  else if (Op.compare_upper(".GE.") == 0)
    Kind = tok::kw_GE;
  else if (Op.compare_upper(".NOT.") == 0)
    Kind = tok::kw_NOT;
  else if (Op.compare_upper(".AND.") == 0)
    Kind = tok::kw_AND;
  else if (Op.compare_upper(".OR.") == 0)
    Kind = tok::kw_OR;
  else if (Op.compare_upper(".EQV.") == 0)
    Kind = tok::kw_EQV;
  else if (Op.compare_upper(".NEQV.") == 0)
    Kind = tok::kw_NEQV;

  return FormTokenWithChars(Result, Kind);
}

/// LexBlankLinesAndComments - Lex blank lines and lines with only
/// comments. Used after we've parsed an ampersand.
void Lexer::LexBlankLinesAndComments() {
  while (true) {
    // Skip blank lines...
    while (isWhitespace(LineBuf[CurPtr]))
      ++CurPtr;

    if (LineBuf[CurPtr] != '\0') {
      if (LineBuf[CurPtr] != '!')
        break;

      // ...and lines with only comments.
      while (LineBuf[CurPtr] != '\0')
        ++CurPtr;
    }

    GetNextLine();
  }
}

/// GetNextCharacter - Get the next character from the buffer ignoring
/// continuation contexts.
char Lexer::GetNextCharacter() {
  if (LineBuf[++CurPtr] != '&')
    return LineBuf[CurPtr];

  ++CurPtr;
  LexBlankLinesAndComments();

  if (LineBuf[CurPtr] != '&')
    return '\0';

  return LineBuf[++CurPtr];
}

/// isPartOfToken - Helper function for LexAmpersandContext. Returns 'true' if
/// the character is correct for the given token being lexed.
bool Lexer::isPartOfToken(Lexer::AmpLexType ALT, char C) {
  switch (ALT) {
  case Lexer::Ident:
    return isIdentifierBody(C);
  case Lexer::Num:
    return isNumberBody(C);
  case Lexer::Hex:
    return isHexNumberBody(C);
  case Lexer::Octal:
    return isOctalNumberBody(C);
  case Lexer::Binary:
    return isBinaryNumberBody(C);
  case Lexer::CharSingleQuote:
  case Lexer::CharDoubleQuote:
    // A character context may be split by an '&'. If so, it must be the last
    // non-whitespace character in the line.
    if (ALT == Lexer::CharDoubleQuote) {
      if (C == '"') {
        C = GetNextCharacter();
        if (C != '"')
          return false;
      }
    } else {
      if (C == '\'') {
        C = GetNextCharacter();
        if (C != '\'')
          return false;
      }
    }

    if (C != '&') return true;

    do {
      C = LineBuf[CurPtr++];
    } while (isHorizontalWhitespace(C));

    return C != '\0';
  }
}

/// LexAmpersandContext - Lex the continuation within a given context.
void Lexer::LexAmpersandContext(AmpLexType ALT) {
  ++CurPtr;
  LexBlankLinesAndComments();

  // Here we have either another '&' or the start of a different token.
  if (LineBuf[CurPtr] != '&')
    return;

  do {
    ++CurPtr;
  } while (LineBuf[CurPtr] != '\0' && LineBuf[CurPtr] != '&' &&
           isPartOfToken(ALT, LineBuf[CurPtr]));

  if (LineBuf[CurPtr] != '&')
    return;

  LexAmpersandContext(ALT);
}

/// LexIdentifier - Lex the remainder of an identifier.
void Lexer::LexIdentifier(Token &Result) {
  // Match [_A-Za-z0-9]*, we have already matched [A-Za-z$]
  unsigned char C = LineBuf[CurPtr];

  while (isIdentifierBody(C))
    C = LineBuf[++CurPtr];

  if (LineBuf[CurPtr] == '&') {
    LexAmpersandContext(Ident);
    Result.setFlag(Token::NeedsCleaning);
  }

  // We let the parser determine what type of identifier this is: identifier,
  // keyword, or built-in function.
  FormTokenWithChars(Result, tok::identifier);
}

/// LexStatementLabel - Lex the remainder of a statement label -- a 5-digit
/// number.
void Lexer::LexStatementLabel(Token &Result) {
  char C = LineBuf[CurPtr];

  while (isDecimalNumberBody(C))
    C = LineBuf[++CurPtr];

  // Update the location of token as well as CurPtr.
  FormTokenWithChars(Result, tok::statement_label);
  Result.setLiteralData(TokStart);
}

/// LexNumericConstant - Lex the remainder of an integer or floating point
/// constant.
void Lexer::LexNumericConstant(Token &Result) {
  char C = LineBuf[CurPtr];
  char PrevCh = '\0';

  while (isNumberBody(C)) {
    PrevCh = C;
    C = LineBuf[++CurPtr];
  }

  if (LineBuf[CurPtr] == '&') {
    LexAmpersandContext(Num);
    Result.setFlag(Token::NeedsCleaning);
  }

  // Could be part of a defined operator. Form numeric constant from what we now
  // have.
  if (PrevCh == '.' && isLetter(LineBuf[CurPtr + 1])) {
    --CurPtr;
    C = LineBuf[CurPtr];
    PrevCh = LineBuf[CurPtr - 1];
  }

  // If we fell out, check for a sign, due to 1e+12.  If we have one, continue.
  if (C == 'E' || C == 'e' || C == 'D' || C == 'd') {
    if (LineBuf[CurPtr + 1] == '-' || LineBuf[CurPtr + 1] == '+')
      ++CurPtr;

    return LexNumericConstant(Result);
  }

  if (PrevCh == '_') {
    // [TODO]: Number constant kind.
    while (isIdentifierBody(C))
      C = LineBuf[++CurPtr];

    --CurPtr;   // Back up over the skipped character.
  }

  if (isIdentifierBody(LineBuf[CurPtr])) {
    while (isIdentifierBody(C))
      C = LineBuf[++CurPtr];

    return FormTokenWithChars(Result, tok::error);
  }

  // Update the location of token as well as CurPtr.
  FormTokenWithChars(Result, tok::numeric_constant);
  Result.setLiteralData(TokStart);
}

/// LexCharacterLiteralConstant - Lex the remainder of a character literal
/// constant (string).
void Lexer::LexCharacterLiteralConstant(Token &Result,
                                        bool DoubleQuotes) {
  char C = LineBuf[CurPtr];
  while (true) {
    if (DoubleQuotes) {
      if (C == '"') {
        if (LineBuf[CurPtr] != '"')
          break;
        C = LineBuf[++CurPtr];
      }
    } else {
      if (C == '\'') {
        if (LineBuf[CurPtr] != '\'')
          break;
        C = LineBuf[++CurPtr];
      }
    }

    if (C == '&')
      break;

    C = LineBuf[++CurPtr];
  }

  if (LineBuf[CurPtr] == '&') {
    LexAmpersandContext(DoubleQuotes ? CharDoubleQuote : CharSingleQuote);
    Result.setFlag(Token::NeedsCleaning);
  }

  // Update the location of token as well as CurPtr.
  FormTokenWithChars(Result, tok::char_literal_constant);
  Result.setLiteralData(TokStart);
}

/// LexComment - Lex a comment and return it, why not?
void Lexer::LexComment(Token &Result) {
  char Char;
  do {
    Char = LineBuf[CurPtr++];
  } while (Char != '\0');

  --CurPtr;
  FormTokenWithChars(Result, tok::comment);
  Result.setLiteralData(TokStart);
}

/// GetNextLine - Get the next line of the program to lex. Return 'false' if
/// there are still more tokens to lex in the current line.
void Lexer::GetNextLine() {
  // Save a pointer to the beginning of the line.
  LineBegin = BufPtr;

  // Fill the "line buffer" with the current line.
  unsigned I = 0;
  while (I != 132 && *BufPtr != '\n' && *BufPtr != '\r' && *BufPtr != '\0')
    LineBuf[I++] = *BufPtr++;
  LineBuf[I] = '\0';

  // Increment the buffer pointer to the start of the next line.
  while (*BufPtr != '\0' && *BufPtr != '\n' && *BufPtr != '\r')
    ++BufPtr;
  while (*BufPtr != '\0' && (*BufPtr == '\n' || *BufPtr == '\r'))
    ++BufPtr;

  // The CurPtr is now at the start of the line.
  CurPtr = 0;
}

/// LexTokenInternal - This implements a simple Fortran family lexer. It is an
/// extremely performance critical piece of code. This assumes that the buffer
/// has a null character at the end of the file. It assumes that the Flags of
/// Result have been cleared before calling this.
void Lexer::LexTokenInternal(Token &Result) {
  // Check to see if there is still more of the line to lex.
  if (LineBuf[CurPtr] == '\0') {
    GetNextLine();

    // The returned token is at the start of the line.
    Result.setFlag(Token::StartOfStatement);
  }

  // Check these flags in this order. We could have a horrid situation like:
  //
  //    a = 1 + 2 ; &
  //    b = 3 + 4
  //
  // And we want 'b' to be marked as StartOfStatement.
  if (LastTokenWasAmpersand) {
    LastTokenWasAmpersand = false;
    Result.clearFlag(Token::StartOfStatement);
  }

  if (LastTokenWasSemicolon) {
    LastTokenWasSemicolon = false;
    Result.setFlag(Token::StartOfStatement);
  }

  // Small amounts of horizontal whitespace is very common between tokens.
  while (isHorizontalWhitespace(LineBuf[CurPtr]))
    ++CurPtr;

  // Read a character, advancing over it.
  TokStart = LineBegin + CurPtr;
  char Char = LineBuf[CurPtr++];
  tok::TokenKind Kind;

  switch (Char) {
  case 0:  // Null.
    // Found end of file?
    if (LineBegin + CurPtr >= CurBuf->getBufferEnd()) {
      Kind = tok::eof;
      break;
    }

    ++CurPtr;
    return LexTokenInternal(Result);
  case '\n':
  case '\r':
  case ' ':
  case '\t':
  case '\f':
  case '\v':
    while (isHorizontalWhitespace(LineBuf[CurPtr]))
      ++CurPtr;
    return LexTokenInternal(Result);

  case '.':
    if (isLetter(LineBuf[CurPtr])) {
      // Match [A-Za-z]*, we have already matched '.'.
      unsigned char C = LineBuf[CurPtr];
      while (isLetter(C))
        C = LineBuf[++CurPtr];

      if (C != '.') {
        // [TODO]: error.
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      C = LineBuf[++CurPtr];
      if (C == '_') {
        // Parse the kind.
        do {
          C = LineBuf[++CurPtr];
        } while (isIdentifierBody(C));
      }

      return FormDefinedOperatorTokenWithChars(Result);
    }
    // FALLTHROUGH
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    // [TODO]: Kinds on literals.
    if (Result.isAtStartOfStatement())
      return LexStatementLabel(Result);
    return LexNumericConstant(Result);

  case '"':
  case '\'':
    // [TODO]: Kinds.
    return LexCharacterLiteralConstant(Result, Char == '"'); 

  // [TODO]: BOZ literals.
  case 'B': case 'b': {
    char C = LineBuf[CurPtr];

    if (C == '"' || C == '\'') { // No whitespace between B and quote.
      // Possible binary constant: B'...', B"..."
      bool DoubleQuote = (C == '"');
      C = LineBuf[++CurPtr];

      while (isBinaryNumberBody(C))
        C = LineBuf[CurPtr++];

      if (C == '&') {
        LexAmpersandContext(Binary);
        Result.setFlag(Token::NeedsCleaning);
        C = LineBuf[CurPtr++];
      }

      if ((LineBegin + CurPtr) - TokStart == 2) {
        //[TODO]: Issue diagnostic. (Empty set of digits for BOZ constant.)
        FormTokenWithChars(Result, tok::error);
        return;
      }

      if ((DoubleQuote && C != '"') || C != '\'') {
        //[TODO]: Issue diagnostic.
        FormTokenWithChars(Result, tok::error);
        return;
      }

      // Update the location of token as well as CurPtr.
      FormTokenWithChars(Result, tok::binary_boz_constant);
      Result.setLiteralData(TokStart);
      return;
    }

    goto LexIdentifier;
  }
  case 'O': case 'o': {
    char C = LineBuf[CurPtr];

    if (C == '"' || C == '\'') { // No whitespace between O and quote.
      // Possible octal constant: O'...', O"..."
      bool DoubleQuote = (C == '"');
      C = LineBuf[++CurPtr];

      while (isOctalNumberBody(C))
        C = LineBuf[CurPtr++];

      if (C == '&') {
        LexAmpersandContext(Octal);
        Result.setFlag(Token::NeedsCleaning);
        C = LineBuf[CurPtr];
      }

      if ((LineBegin + CurPtr) - TokStart == 2) {
        //[TODO]: Issue diagnostic. (Empty set of digits for BOZ constant.)
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      if ((DoubleQuote && C != '"') || C != '\'') {
        //[TODO]: Issue diagnostic.
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      // Update the location of token as well as CurPtr.
      FormTokenWithChars(Result, tok::octal_boz_constant);
      Result.setLiteralData(TokStart);
      return;
    }

    goto LexIdentifier;
  }
  case 'Z': case 'z': {
    char C = LineBuf[CurPtr];

    if (C == '"' || C == '\'') { // No whitespace between Z and quote.
      // Possible hexadecimal constant: Z'...', Z"..."
      bool DoubleQuote = (C == '"');
      C = LineBuf[++CurPtr];

      while (isHexNumberBody(C))
        C = LineBuf[CurPtr++];

      if (C == '&') {
        LexAmpersandContext(Hex);
        Result.setFlag(Token::NeedsCleaning);
        C = LineBuf[CurPtr];
      }

      if ((LineBegin + CurPtr) - TokStart == 2) {
        //[TODO]: Issue diagnostic. (Empty set of digits for BOZ constant.)
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      if ((DoubleQuote && C != '"') || C != '\'') {
        //[TODO]: Issue diagnostic.
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      // Update the location of token as well as CurPtr.
      FormTokenWithChars(Result, tok::hex_boz_constant);
      Result.setLiteralData(TokStart);
      return;
    }

    goto LexIdentifier;
  }
  case 'A': /* 'B' */ case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
  /* 'O' */ case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
  case 'V': case 'W': case 'X': case 'Y': /* 'Z' */
  case 'a': /* 'b' */ case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  /* 'o' */ case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': case 'x': case 'y': /* 'z' */
LexIdentifier:
    return LexIdentifier(Result);

  case '!':
    LexComment(Result);
    if (Features.ReturnComments)
      return;
    return LexTokenInternal(Result);

  case '&':
    LastTokenWasAmpersand = true;
    LexBlankLinesAndComments();
    return LexTokenInternal(Result);

  // [TODO]: Special Characters.
  case '[':
    Kind = tok::l_square;
    break;
  case ']':
    Kind = tok::r_square;
    break;
  case '(':
    Char = LineBuf[CurPtr];
    if (Char == '/') {
      // Beginning of array initialization.
      Kind = tok::l_parenslash;
      ++CurPtr;
    } else {
      Kind = tok::l_paren;
    }
    break;
  case ')':
    Kind = tok::r_paren;
    break;
  case '{':
    Kind = tok::l_brace;
    break;
  case '}':
    Kind = tok::r_brace;
    break;
  case ',':
    Kind = tok::comma;
    break;
  case ':':
    Char = LineBuf[CurPtr];
    if (Char == ':') {
      Kind = tok::coloncolon;
      ++CurPtr;
    } else {
      Kind = tok::colon;
    }
    break;
  case ';':
    LastTokenWasSemicolon = true;
    return LexTokenInternal(Result);
  case '%':
    Kind = tok::percent;
    break;
  case '~':
    Kind = tok::tilde;
    break;
  case '?':
    Kind = tok::question;
    break;
  case '`':
    Kind = tok::backtick;
    break;
  case '^':
    Kind = tok::caret;
    break;
  case '|':
    Kind = tok::pipe;
    break;
  case '$':
    Kind = tok::dollar;
    break;
  case '#':
    Kind = tok::hash;
    break;
  case '@':
    Kind = tok::at;
    break;
  // [TODO]: Arithmetical Operators
  case '+':
    Kind = tok::plus;
    break;
  case '-':
    Kind = tok::minus;
    break;
  case '*':
    Char = LineBuf[CurPtr];
    if (Char == '*') {
      // Power operator.
      Kind = tok::starstar;
      ++CurPtr;
    } else {
      Kind = tok::star;
    }
    break;
  case '/':
    Char = LineBuf[CurPtr];
    if (Char == '=') {
      // Not equal operator.
      Kind = tok::slashequal;
      ++CurPtr;
    } else if (Char == ')') {
      // End of array initialization list.
      Kind = tok::slashr_paren;
      ++CurPtr;
    } else if (Char == '/') {
      // Concatenation operator.
      Kind = tok::slashslash;
      ++CurPtr;
    } else {
      Kind = tok::slash;
    }
    break;
  // [TODO]: Logical Operators
  case '=':
    Char = LineBuf[CurPtr];
    if (Char == '=') {
      Kind = tok::equalequal;
      ++CurPtr;
    } else if (Char == '>') {
      Kind = tok::equalgreater;
      ++CurPtr;
    } else {      
      Kind = tok::equal;
    }
    break;
  case '<':
    Char = LineBuf[CurPtr];
    if (Char == '=') {
      Kind = tok::lessequal;
      ++CurPtr;
    } else {
      Kind = tok::less;
    }
    break;
  case '>':
    Char = LineBuf[CurPtr];
    if (Char == '=') {
      Kind = tok::greaterequal;
      ++CurPtr;
    } else {
      Kind = tok::greater;
    }
    break;
  default:
    TokStart = LineBegin + CurPtr;
    Kind = tok::error;
    break;
  }

  // Update the location of token as well as LexPtr.
  FormTokenWithChars(Result, Kind);
}
