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
#include "flang/Basic/Diagnostic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/Twine.h"
using namespace flang;

static void InitCharacterInfo();
static bool isWhitespace(unsigned char c);
static bool isHorizontalWhitespace(unsigned char c);
static bool isVerticalWhitespace(unsigned char c);

Lexer::Lexer(llvm::SourceMgr &SM, const LangOptions &features, Diagnostic &D)
  : Text(D), Diags(D), SrcMgr(SM), Features(features), TokStart(0),
    LastTokenWasSemicolon(false) {
  InitCharacterInfo();
}

void Lexer::setBuffer(const llvm::MemoryBuffer *Buf, const char *Ptr) {
  Text.SetBuffer(Buf, Ptr);
  CurBuf = Buf;
  TokStart = 0;
}

llvm::SMLoc Lexer::getLoc() const {
  return llvm::SMLoc::getFromPointer(TokStart);
}

void Lexer::LineOfText::
SetBuffer(const llvm::MemoryBuffer *Buf, const char *Ptr) {
  BufPtr = (Ptr ? Ptr : Buf->getBufferStart());
  CurAtom = CurPtr = 0;
  Atoms.clear();
  GetNextLine();
}

/// SkipBlankLinesAndComments - Helper function that skips blank lines and lines
/// with only comments.
bool Lexer::LineOfText::
SkipBlankLinesAndComments(unsigned &I, const char *&LineBegin) {
  // Skip blank lines and lines with only comments.
  while (isVerticalWhitespace(*BufPtr) && *BufPtr != '\0')
    ++BufPtr;

  while (I != 132 && isHorizontalWhitespace(*BufPtr) && *BufPtr != '\0')
    ++I, ++BufPtr;

  if (I != 132 && *BufPtr == '!') {
    do {
      ++BufPtr;
    } while (!isVerticalWhitespace(*BufPtr));

    while (isVerticalWhitespace(*BufPtr))
      ++BufPtr;

    // Save a pointer to the beginning of the line.
    LineBegin = BufPtr;
    I = 0;
    SkipBlankLinesAndComments(I, LineBegin);
  }

  // If we have a continuation character at the beginning of the line, and we've
  // had a previous continuation character at the end of the line, then readjust
  // the LineBegin.
  if (I != 132 && *BufPtr == '&') {
    if (Atoms.empty()) // FIXME: This isn't sufficient.
      Diags.ReportError(SMLoc::getFromPointer(BufPtr),
                        "continuation character used out of context");
    ++I, ++BufPtr;
    LineBegin = BufPtr;
    return true;
  }

  return false;
}

/// GetCharacterLiteral - A character literal has to be treated specially
/// because an ampersand may exist within it.
void Lexer::LineOfText::
GetCharacterLiteral(unsigned &I, const char *&LineBegin) {
  // Skip blank lines and lines with only comments.
  SkipBlankLinesAndComments(I, LineBegin);

  const char *AmpersandPos = 0;
  const char *QuoteStart = BufPtr;
  bool DoubleQuotes = (*BufPtr == '"');
  ++I, ++BufPtr;
  while (I != 132 && !isVerticalWhitespace(*BufPtr) && *BufPtr != '\0') {
    if (*BufPtr == '"' || *BufPtr == '\'') {
      ++I, ++BufPtr;
      if (DoubleQuotes) {
        if (I != 132 && *BufPtr == '"')
          continue;
      } else {
        if (I != 132 && *BufPtr == '\'')
          continue;
      }

      return;
    }

    if (*BufPtr != '&') goto next_char;

    AmpersandPos = BufPtr;
    ++I, ++BufPtr;
    if (I == 132)
      break;
    while (I != 132 && isHorizontalWhitespace(*BufPtr) && *BufPtr != '\0')
      ++I, ++BufPtr;

    if (I == 132 || isVerticalWhitespace(*BufPtr) || *BufPtr == '\0')
      break;
    AmpersandPos = 0;

  next_char:
    ++I, ++BufPtr;
  }

  if (AmpersandPos)
    Atoms.push_back(StringRef(LineBegin, AmpersandPos - LineBegin));
  else
    Diags.ReportError(SMLoc::getFromPointer(QuoteStart),
                      "unterminated character literal");

  LineBegin = BufPtr;
  I = 0;
  GetCharacterLiteral(I, LineBegin);
}

const char *Lexer::LineOfText::Padding = " ";

/// GetNextLine - Get the next line of the program to lex.
void Lexer::LineOfText::GetNextLine() {
  // Save a pointer to the beginning of the line.
  const char *LineBegin = BufPtr;

  // Fill the line buffer with the current line.
  unsigned I = 0;

  // Skip blank lines and lines with only comments.
  bool BeginsWithAmp = SkipBlankLinesAndComments(I, LineBegin);

  const char *AmpersandPos = 0;
  while (I != 132 && !isVerticalWhitespace(*BufPtr) && *BufPtr != '\0') {
    if (*BufPtr == '\'' || *BufPtr == '"') {
      // TODO: A BOZ constant doesn't get parsed like a character literal.
      GetCharacterLiteral(I, LineBegin);
      if (I == 132 || isVerticalWhitespace(*BufPtr))
        break;
    } else if (*BufPtr == '&') {
      AmpersandPos = BufPtr;
      do {
        ++I, ++BufPtr;
      } while (I != 132 && isHorizontalWhitespace(*BufPtr) && *BufPtr!='\0');

      // We should be either at the end of the line, at column 132, or at the
      // beginning of a comment. If not, the '&' is invalid. Report and ignore
      // it.
      if (I != 132 && !isVerticalWhitespace(*BufPtr) && *BufPtr != '!') {
        Diags.ReportError(SMLoc::getFromPointer(AmpersandPos),
                          "continuation character not at end of line");
        AmpersandPos = 0;     // Pretend nothing's wrong.
      }

      if (*BufPtr == '!') {
        // Eat the comment after a continuation.
        while (!isVerticalWhitespace(*BufPtr) && *BufPtr != '\0')
          ++BufPtr;

        break;
      }

      if (I == 132 || isVerticalWhitespace(*BufPtr))
        break;
    }

    ++I, ++BufPtr;
  }

  if (AmpersandPos) {
    Atoms.push_back(StringRef(LineBegin, AmpersandPos - LineBegin));
  } else {
    if (!BeginsWithAmp && !Atoms.empty())
      // This is a line that doesn't start with an '&'. The tokens are not
      // contiguous. Insert a space to indicate this.
      Atoms.push_back(StringRef(Padding));
    Atoms.push_back(StringRef(LineBegin, BufPtr - LineBegin));
  }

  // Increment the buffer pointer to the start of the next line.
  while (*BufPtr != '\0' && !isVerticalWhitespace(*BufPtr))
    ++BufPtr;
  while (*BufPtr != '\0' && isVerticalWhitespace(*BufPtr))
    ++BufPtr;

  if (AmpersandPos)
    GetNextLine();
}

char Lexer::LineOfText::GetNextChar() {
  StringRef Atom = Atoms[CurAtom];
  if (CurPtr + 1 >= Atom.size()) {
    if (CurAtom + 1 >= Atoms.size()) {
      if (CurPtr != Atom.size()) ++CurPtr;
      return '\0';
    }
    Atom = Atoms[++CurAtom];
    CurPtr = 0;
    return Atom.data()[CurPtr];
  }
  assert(!Atom.empty() && "Atom has no contents!");
  return Atom.data()[++CurPtr];
}

char Lexer::LineOfText::GetCurrentChar() const {
  StringRef Atom = Atoms[CurAtom];
  if (CurPtr == Atom.size())
    return '\0';
  assert(!Atom.empty() && "Atom has no contents!");
  return Atom.data()[CurPtr];
}

char Lexer::LineOfText::PeekNextChar() const {
  StringRef Atom = Atoms[CurAtom];
  if (CurPtr + 1 == Atom.size()) {
    if (CurAtom + 1 == Atoms.size())
      return '\0';
    return Atoms[CurAtom + 1][0];
  }
  assert(!Atom.empty() && "Atom has no contents!");
  return Atom.data()[CurPtr + 1];
}

char Lexer::LineOfText::PeekPrevChar() const {
  if (Atoms.empty()) return '\0';
  StringRef Atom = Atoms[CurAtom];
  if (CurPtr == 0) {
    if (CurAtom == 0)
      return '\0';
    return Atoms[CurAtom - 1].back();
  }
  assert(!Atom.empty() && "Atom has no contents!");
  return Atom.data()[CurPtr - 1];
}

void Lexer::LineOfText::dump() const {
  dump(llvm::errs());
}

void Lexer::LineOfText::dump(raw_ostream &OS) const {
  for (SmallVectorImpl<StringRef>::const_iterator
         I = Atoms.begin(), E = Atoms.end(); I != E; ++I)
    OS << *I;
  OS << '\n';
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
/// at the current pointer, going to TokEnd that forms the token. This method
/// takes that range and assigns it to the token as its location and size. In
/// addition, since tokens cannot overlap.
void Lexer::FormTokenWithChars(Token &Result, tok::TokenKind Kind) {
  uint64_t TokLen = getCurrentPtr() - TokStart;
  CurKind = Kind;
  Result.setLocation(llvm::SMLoc::getFromPointer(TokStart));
  Result.setLength(TokLen);
  Result.setKind(Kind);

  if (!Text.IsInCurrentAtom(TokStart))
    Result.setFlag(Token::NeedsCleaning);
}

/// FormDefinedOperatorTokenWithChars - A special form of FormTokenWithChars. It
/// will see if the defined operator is an intrinsic operator. If so, it will
/// set the token's kind to that value.
void Lexer::FormDefinedOperatorTokenWithChars(Token &Result) {
  unsigned TokLen = getCurrentPtr() - TokStart;
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

  if (Op.compare_lower(".TRUE.") == 0 || Op.compare_lower(".FALSE.") == 0)
    Kind = tok::logical_literal_constant;
  else if (Op.compare_lower(".EQ.") == 0)
    Kind = tok::kw_EQ;
  else if (Op.compare_lower(".NE.") == 0)
    Kind = tok::kw_NE;
  else if (Op.compare_lower(".LT.") == 0)
    Kind = tok::kw_LT;
  else if (Op.compare_lower(".LE.") == 0)
    Kind = tok::kw_LE;
  else if (Op.compare_lower(".GT.") == 0)
    Kind = tok::kw_GT;
  else if (Op.compare_lower(".GE.") == 0)
    Kind = tok::kw_GE;
  else if (Op.compare_lower(".NOT.") == 0)
    Kind = tok::kw_NOT;
  else if (Op.compare_lower(".AND.") == 0)
    Kind = tok::kw_AND;
  else if (Op.compare_lower(".OR.") == 0)
    Kind = tok::kw_OR;
  else if (Op.compare_lower(".EQV.") == 0)
    Kind = tok::kw_EQV;
  else if (Op.compare_lower(".NEQV.") == 0)
    Kind = tok::kw_NEQV;

  return FormTokenWithChars(Result, Kind);
}

/// LexIdentifier - Lex the remainder of an identifier.
void Lexer::LexIdentifier(Token &Result) {
  // Match [_A-Za-z0-9]*, we have already matched [A-Za-z$]
  unsigned char C = getCurrentChar();

  while (isIdentifierBody(C))
    C = getNextChar();

  // We let the parser determine what type of identifier this is: identifier,
  // keyword, or built-in function.
  FormTokenWithChars(Result, tok::identifier);
}

/// LexStatementLabel - Lex the remainder of a statement label -- a 5-digit
/// number.
void Lexer::LexStatementLabel(Token &Result) {
  char C = getNextChar();

  while (isDecimalNumberBody(C))
    C = getNextChar();

  // Update the location of token.
  FormTokenWithChars(Result, tok::statement_label);
  Result.setLiteralData(TokStart);
}

/// LexIntegerLiteralConstant - Lex an integer literal constant.
///
///   [R406]:
///     signed-int-literal-constant :=
///         [ sign ] int-literal-constant
///   [R407]:
///     int-literal-constant :=
///         digit-string [ _kind-param ]
///   [R410]:
///     digit-string :=
///         digit [ digit ] ...
bool Lexer::LexIntegerLiteralConstant() {
  bool IntPresent = false;
  char C = getNextChar();
  if (C == '-' || C == '+')
    C = getNextChar();

  while (isDecimalNumberBody(C)) {
    IntPresent = true;
    C = getNextChar();
  }

  return IntPresent;
}

/// LexNumericConstant - Lex an integer or floating point constant.
void Lexer::LexNumericConstant(Token &Result) {
  const char *NumBegin = getCurrentPtr();
  bool BeginsWithDot = (*NumBegin == '.');
  if (!LexIntegerLiteralConstant() && BeginsWithDot) {
    Diags.ReportError(SMLoc::getFromPointer(NumBegin),
                      "invalid REAL literal");
    FormTokenWithChars(Result, tok::error);
    return;
  }

  bool IsReal = false;
  char PrevChar = getCurrentChar();
  if (PrevChar == '.') {
    IsReal = true;
    getNextChar();
    if (LexIntegerLiteralConstant())
      PrevChar = '\0';
  }

  // Could be part of a defined operator. Form numeric constant from what we now
  // have.
  char C = getCurrentChar();
  if (PrevChar == '.' && isLetter(C)) {
    C = getCurrentChar();
    if (isLetter(C)) {
      if (!BeginsWithDot)
        IsReal = false;
      goto make_literal;
    }
  }

  if (C == 'E' || C == 'e' || C == 'D' || C == 'd') {
    IsReal = true;
    C = getNextChar();
    if (C == '-' || C == '+')
      C = getNextChar();
    if (!isDecimalNumberBody(C)) {
      Diags.ReportError(SMLoc::getFromPointer(NumBegin),
                        "invalid REAL literal");
      FormTokenWithChars(Result, tok::error);
      return;
    }
    LexIntegerLiteralConstant();
  }

  if (C == '_')
    do {
      C = getNextChar();
    } while (isIdentifierBody(C) || isDecimalNumberBody(C));

  // Update the location of token.
 make_literal:
  if (!IsReal)
    FormTokenWithChars(Result, tok::int_literal_constant);
  else
    FormTokenWithChars(Result, tok::real_literal_constant);
  Result.setLiteralData(TokStart);
}

/// LexCharacterLiteralConstant - Lex the remainder of a character literal
/// constant (string).
void Lexer::LexCharacterLiteralConstant(Token &Result,
                                        bool DoubleQuotes) {
  while (true) {
    char C = getNextChar();
    if (C == '\0') break;

    if (DoubleQuotes) {
      if (C == '"') {
        if (peekNextChar() != '"') {
          getNextChar();
          break;
        }
        C = getNextChar();
      }
    } else {
      if (C == '\'') {
        if (peekNextChar() != '\'') {
          getNextChar();
          break;
        }
        C = getNextChar();
      }
    }
  }

  // Update the location of token.
  FormTokenWithChars(Result, tok::char_literal_constant);
  Result.setLiteralData(TokStart);
}

/// LexComment - Lex a comment and return it, why not?
void Lexer::LexComment(Token &Result) {
  char C;
  do {
    C = getNextChar();
  } while (C != '\0');

  FormTokenWithChars(Result, tok::comment);
  Result.setLiteralData(TokStart);
}

/// LexTokenInternal - This implements a simple Fortran family lexer. It is an
/// extremely performance critical piece of code. This assumes that the buffer
/// has a null character at the end of the file. It assumes that the Flags of
/// Result have been cleared before calling this.
void Lexer::LexTokenInternal(Token &Result) {
  // Check to see if there is still more of the line to lex.
  if (Text.empty() || Text.AtEndOfLine()) {
    Text.Reset();
    Text.GetNextLine();
  }

  // Check to see if we're at the start of a line.
  if (getLineBegin() == getCurrentPtr())
    // The returned token is at the start of the line.
    Result.setFlag(Token::StartOfStatement);

  // If we saw a semicolon, then we're at the start of a new statement.
  if (LastTokenWasSemicolon) {
    LastTokenWasSemicolon = false;
    Result.setFlag(Token::StartOfStatement);
  }

  // Small amounts of horizontal whitespace is very common between tokens.
  char Char = getCurrentChar();
  while (isHorizontalWhitespace(Char))
    Char = getNextChar();

  TokStart = getCurrentPtr();
  tok::TokenKind Kind;

  switch (Char) {
  case 0:  // Null.
    // Found end of file?
    if (getCurrentPtr() >= CurBuf->getBufferEnd()) {
      Kind = tok::eof;
      break;
    }

    getNextChar();
    return LexTokenInternal(Result);
  case '\n':
  case '\r':
  case ' ':
  case '\t':
  case '\f':
  case '\v':
    do {
      Char = getNextChar();
    } while (isHorizontalWhitespace(Char));
    return LexTokenInternal(Result);

  case '.':
    Char = getNextChar();
    if (isLetter(Char)) {
      // Match [A-Za-z]*, we have already matched '.'.
      while (isLetter(Char))
        Char = getNextChar();

      if (Char != '.') {
        // [TODO]: error.
        Diags.ReportError(SMLoc::getFromPointer(TokStart),
                          "invalid defined operator missing end '.'");
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      Char = getNextChar();
      if (Char == '_') {
        // Parse the kind.
        do {
          Char = getNextChar();
        } while (isIdentifierBody(Char) || isDecimalNumberBody(Char));
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
  case 'B': case 'b':
    if (Char == '"' || Char == '\'') { // No whitespace between B and quote.
      // Possible binary constant: B'...', B"..."
      const char *BOZBegin = getCurrentPtr();
      bool DoubleQuote = (Char == '"');

      do {
        Char = getNextChar();
      } while (isBinaryNumberBody(Char));

      if (getCurrentPtr() - TokStart == 2) {
        Diags.ReportError(SMLoc::getFromPointer(BOZBegin),
                          "no binary digits for BOZ constant");
        FormTokenWithChars(Result, tok::error);
        return;
      }

      if ((DoubleQuote && Char != '"') || Char != '\'') {
        Diags.ReportError(SMLoc::getFromPointer(BOZBegin),
                          "binary BOZ constant missing ending quote");
        FormTokenWithChars(Result, tok::error);
        return;
      }

      // Update the location of token.
      FormTokenWithChars(Result, tok::binary_boz_constant);
      Result.setLiteralData(TokStart);
      return;
    }

    goto LexIdentifier;
  case 'O': case 'o':
    if (Char == '"' || Char == '\'') { // No whitespace between O and quote.
      // Possible octal constant: O'...', O"..."
      const char *BOZBegin = getCurrentPtr();
      bool DoubleQuote = (Char == '"');

      do {
        Char = getNextChar();
      } while (isOctalNumberBody(Char));

      if (getCurrentPtr() - TokStart == 2) {
        Diags.ReportError(SMLoc::getFromPointer(BOZBegin),
                          "no octal digits for BOZ constant");
        FormTokenWithChars(Result, tok::error);
        return;
      }

      if ((DoubleQuote && Char != '"') || Char != '\'') {
        Diags.ReportError(SMLoc::getFromPointer(BOZBegin),
                          "octal BOZ constant missing ending quote");
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      // Update the location of token.
      FormTokenWithChars(Result, tok::octal_boz_constant);
      Result.setLiteralData(TokStart);
      return;
    }

    goto LexIdentifier;
  case 'X': case 'x':
  case 'Z': case 'z':
    if (Char == '"' || Char == '\'') { // No whitespace between Z and quote.
      // Possible hexadecimal constant: Z'...', Z"..."
      const char *BOZBegin = getCurrentPtr();
      bool DoubleQuote = (Char == '"');

      do {
        Char = getNextChar();
      } while (isHexNumberBody(Char));

      if (getCurrentPtr() - TokStart == 2) {
        Diags.ReportError(SMLoc::getFromPointer(BOZBegin),
                          "no hex digits for BOZ constant");
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      if ((DoubleQuote && Char != '"') || Char != '\'') {
        Diags.ReportError(SMLoc::getFromPointer(BOZBegin),
                          "hex BOZ constant missing ending quote");
        FormTokenWithChars(Result, tok::unknown);
        return;
      }

      // Update the location of token.
      FormTokenWithChars(Result, tok::hex_boz_constant);
      Result.setLiteralData(TokStart);
      return;
    }

    goto LexIdentifier;
  case 'A': /* 'B' */ case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
  /* 'O' */ case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
  case 'V': case 'W': /* 'X' */ case 'Y': /* 'Z' */
  case 'a': /* 'b' */ case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  /* 'o' */ case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': /* 'x' */ case 'y': /* 'z' */
LexIdentifier:
    return LexIdentifier(Result);

  case '!':
    LexComment(Result);
    if (Features.ReturnComments)
      return;
    return LexTokenInternal(Result);

  // [TODO]: Special Characters.
  case '[':
    Kind = tok::l_square;
    break;
  case ']':
    Kind = tok::r_square;
    break;
  case '(':
    Char = peekNextChar();
    if (Char == '/') {
      // beginning of array initialization.
      Kind = tok::l_parenslash;
      Char = getNextChar();
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
    Char = peekNextChar();
    if (Char == ':') {
      Kind = tok::coloncolon;
      Char = getNextChar();
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
    Char = peekNextChar();
    if (Char == '*') {
      // Power operator.
      Kind = tok::starstar;
      Char = getNextChar();
    } else {
      Kind = tok::star;
    }
    break;
  case '/':
    Char = peekNextChar();
    if (Char == '=') {
      // Not equal operator.
      Kind = tok::slashequal;
      Char = getNextChar();
    } else if (Char == ')') {
      // End of array initialization list.
      Kind = tok::slashr_paren;
      Char = getNextChar();
    } else if (Char == '/') {
      // Concatenation operator.
      Kind = tok::slashslash;
      Char = getNextChar();
    } else {
      Kind = tok::slash;
    }
    break;
  // [TODO]: Logical Operators
  case '=':
    Char = peekNextChar();
    if (Char == '=') {
      Kind = tok::equalequal;
      Char = getNextChar();
    } else if (Char == '>') {
      Kind = tok::equalgreater;
      Char = getNextChar();
    } else {      
      Kind = tok::equal;
    }
    break;
  case '<':
    Char = peekNextChar();
    if (Char == '=') {
      Kind = tok::lessequal;
      Char = getNextChar();
    } else {
      Kind = tok::less;
    }
    break;
  case '>':
    Char = peekNextChar();
    if (Char == '=') {
      Kind = tok::greaterequal;
      Char = getNextChar();
    } else {
      Kind = tok::greater;
    }
    break;
  default:
    TokStart = getCurrentPtr();
    Kind = tok::error;
    break;
  }

  // Update the location of token as well as LexPtr.
  Char = getNextChar();
  FormTokenWithChars(Result, Kind);
}
