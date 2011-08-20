//===-- ParserDecl.cpp - Fortran Declaration Parser -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Fortran declaration parsing.
//
//===----------------------------------------------------------------------===//

#include "flang/Parse/Parser.h"
#include "flang/AST/Decl.h"
#include "flang/AST/Expr.h"
#include "flang/Basic/Actions.h"
using namespace fortran;

/// AssignAttrSpec - Helper function that assigns the attribute specification to
/// the list, but reports an error if that attribute was all ready assigned.
bool Parser::AssignAttrSpec(DeclSpec *DS, DeclSpec::AttrSpec Val) {
  if (DS->hasAttribute(Val))
    return Diag.ReportError(Tok.getLocation(),
                            "attribute specification defined more than once");
  DS->setAttribute(Val);
  Lex();
  return false;
}

/// AssignAccessSpec - Helper function that assigns the access specification to
/// the DeclSpec, but reports an error if that access spec was all ready
/// assigned.
bool Parser::AssignAccessSpec(DeclSpec *DS, DeclSpec::AccessSpec Val) {
  if (DS->hasAccessControl(Val))
    return Diag.ReportError(Tok.getLocation(),
                            "access specification defined more than once");
  DS->setAccessControl(Val);
  Lex();
  return false;
}

/// AssignIntentSpec - Helper function that assigns the intent specification to
/// the DeclSpec, but reports an error if that intent spec was all ready
/// assigned.
bool Parser::AssignIntentSpec(DeclSpec *DS, DeclSpec::IntentSpec Val) {
  if (DS->hasIntent(Val))
    return Diag.ReportError(Tok.getLocation(),
                            "intent specification defined more than once");
  DS->setIntent(Val);
  Lex();
  return false;
}

/// ParseTypeDeclarationStmt - Parse a type-declaration-stmt construct.
///
///   [5.2.1] R501:
///     type-declaration-stmt :=
///         declaration-type-spec [ [ , attr-spec ] ... :: ] entity-decl-list
bool Parser::ParseTypeDeclarationStmt() {
  if (!Tok.isAtStartOfStatement())
    return true;
  
  DeclSpec *DS = 0;
  if (ParseDeclarationTypeSpec(DS))
    return true;

  llvm::SmallVector<ExprResult, 4> Dimensions;
  while (EatIfPresent(tok::comma)) {
    // [5.2.1] R502:
    //   attr-spec :=
    //       access-spec
    //    or ALLOCATABLE
    //    or ASYNCHRONOUS
    //    or CODIMENSION lbracket coarray-spec rbracket
    //    or CONTIGUOUS
    //    or DIMENSION ( array-spec )
    //    or EXTERNAL
    //    or INTENT ( intent-spec )
    //    or INTRINSIC
    //    or language-binding-spec
    //    or OPTIONAL
    //    or PARAMETER
    //    or POINTER
    //    or PROTECTED
    //    or SAVE
    //    or TARGET
    //    or VALUE
    //    or VOLATILE
    switch (Tok.getKind()) {
    default:
      Diag.ReportError(Tok.getLocation(),
                       "unknown attribute specification");
      goto error;
    case tok::kw_ALLOCATABLE:
      if (AssignAttrSpec(DS, DeclSpec::AS_Allocatable))
        goto error;
      break;
    case tok::kw_ASYNCHRONOUS:
      if (AssignAttrSpec(DS, DeclSpec::AS_Asynchronous))
        goto error;
      break;
    case tok::kw_CODIMENSION:
      if (AssignAttrSpec(DS, DeclSpec::AS_Codimension))
        goto error;
      if (!EatIfPresent(tok::l_square)) {
        Diag.ReportError(Tok.getLocation(),
                         "expected '[' in CODIMENSION attribute");
        goto error;
      }

      // FIXME: Parse the coarray-spec.

      if (!EatIfPresent(tok::r_square)) {
        Diag.ReportError(Tok.getLocation(),
                         "expected ']' in CODIMENSION attribute");
        goto error;
      }

      break;
    case tok::kw_CONTIGUOUS:
      if (AssignAttrSpec(DS, DeclSpec::AS_Contiguous))
        goto error;
      break;
    case tok::kw_DIMENSION:
      if (AssignAttrSpec(DS, DeclSpec::AS_Dimension))
        goto error;
      if (ParseArraySpec(Dimensions))
        goto error;
      break;
    case tok::kw_EXTERNAL:
      if (AssignAttrSpec(DS, DeclSpec::AS_External))
        goto error;
      break;
    case tok::kw_INTENT:
      if (AssignAttrSpec(DS, DeclSpec::AS_Intent))
        goto error;

      if (!EatIfPresent(tok::l_paren)) {
        Diag.ReportError(Tok.getLocation(),
                         "expected '(' after 'INTENT' keyword");
        goto error;
      }

      switch (Tok.getKind()) {
      default:
        Diag.ReportError(Tok.getLocation(),
                         "invalid INTENT specifier");
        goto error;
      case tok::kw_IN:
        if (AssignIntentSpec(DS, DeclSpec::IS_In))
          goto error;
        break;
      case tok::kw_OUT:
        if (AssignIntentSpec(DS, DeclSpec::IS_Out))
          goto error;
        break;
      case tok::kw_INOUT:
        if (AssignIntentSpec(DS, DeclSpec::IS_InOut))
          goto error;
        break;
      }
      Lex();

      if (!EatIfPresent(tok::r_paren)) {
        Diag.ReportError(Tok.getLocation(),
                         "expected ')' after INTENT specifier");
        goto error;
      }

      break;
    case tok::kw_INTRINSIC:
      if (AssignAttrSpec(DS, DeclSpec::AS_Intrinsic))
        goto error;
      break;
    case tok::kw_OPTIONAL:
      if (AssignAttrSpec(DS, DeclSpec::AS_Optional))
        goto error;
      break;
    case tok::kw_PARAMETER:
      if (AssignAttrSpec(DS, DeclSpec::AS_Parameter))
        goto error;
      break;
    case tok::kw_POINTER:
      if (AssignAttrSpec(DS, DeclSpec::AS_Pointer))
        goto error;
      break;
    case tok::kw_PROTECTED:
      if (AssignAttrSpec(DS, DeclSpec::AS_Protected))
        goto error;
      break;
    case tok::kw_SAVE:
      if (AssignAttrSpec(DS, DeclSpec::AS_Save))
        goto error;
      break;
    case tok::kw_TARGET:
      if (AssignAttrSpec(DS, DeclSpec::AS_Target))
        goto error;
      break;
    case tok::kw_VALUE:
      if (AssignAttrSpec(DS, DeclSpec::AS_Value))
        goto error;
      break;
    case tok::kw_VOLATILE:
      if (AssignAttrSpec(DS, DeclSpec::AS_Volatile))
        goto error;
      break;

    // Access Control Specifiers
    case tok::kw_PUBLIC:
      if (AssignAccessSpec(DS, DeclSpec::AC_Public))
        goto error;
      break;
    case tok::kw_PRIVATE:
      if (AssignAccessSpec(DS, DeclSpec::AC_Private))
        goto error;
      break;
    }
  }

  EatIfPresent(tok::coloncolon);

  while (!Tok.isAtStartOfStatement()) {
    llvm::SMLoc IDLoc = Tok.getLocation();
    const IdentifierInfo *ID = Tok.getIdentifierInfo();
    if (!ID) {
      Diag.ReportError(IDLoc,
                       "expected an identifier in TYPE list");
      goto error;
    }

    if (Context.getVarDecl(ID)) {
      // This variable has been defined before.
      Diag.ReportError(IDLoc,
                       "identifier already was declared");
      // FIXME: Make this error better!
    } else {
      Actions.ActOnVarDecl(&Context, DS, Context.getOrCreateVarDecl(ID));
    }

    Lex();
    llvm::SMLoc CommaLoc = Tok.getLocation();
    if (!EatIfPresent(tok::comma)) {
      if (!Tok.isAtStartOfStatement()) {
        Diag.ReportError(Tok.getLocation(),
                         "expected a ',' in TYPE list");
        goto error;
      }

      break;
    }

    if (Tok.isAtStartOfStatement()) {
      Diag.ReportError(CommaLoc,
                       "expected an identifier after ',' in TYPE list");
      goto error;
    }
  }

  return false;
 error:
  for (llvm::SmallVectorImpl<ExprResult>::iterator
         I = Dimensions.begin(), E = Dimensions.end(); I != E; ++I)
    delete I->take();
  delete DS;
  return true;
}

/// Parse the optional KIND selector.
/// 
///   [4.4.1] R405:
///     kind-selector :=
///         ( [ KIND = ] scalar-int-initialization-expr )
bool Parser::ParseKindSelector(Selector &Kind) {
  if (EatIfPresent(tok::kw_KIND)) {
    if (!EatIfPresent(tok::equal)) {
      if (Tok.isNot(tok::l_paren))
        return Diag.ReportError(Tok.getLocation(), "invalid kind selector");

      // TODO: We have a "REAL (KIND(10D0)) :: x" situation.
      return false;
    }
  }

  ExprResult KindExpr = ParseExpression();
  if (KindExpr.isInvalid())
    return true;

  Kind.setKindExpr(KindExpr);
  return false;
}

/// Parse the optional LEN selector.
/// 
///   [4.4.4.1] R425:
///     length-selector :=
///         ( [ LEN = ] type-param-value )
bool Parser::ParseLengthSelector(Selector &Len) {
  if (EatIfPresent(tok::kw_LEN) && !EatIfPresent(tok::equal))
    return Diag.ReportError(Tok.getLocation(), "invalid length selector");

  ExprResult KindExpr = ParseExpression();
  if (KindExpr.isInvalid())
    return true;

  Len.setKindExpr(KindExpr);
  return false;
}

/// ParseDerivedTypeSpec - Parse the type declaration specifier.
///
///   [4.5.8] R455:
///     derived-type-spec :=
///         type-name [ ( type-param-spec-list ) ]
///
///   [4.5.8] R456:
///     type-param-spec :=
///         [ keyword = ] type-param-value
bool Parser::ParseDerivedTypeSpec(DeclSpec *&DS) {
  llvm::SMLoc Loc = Tok.getLocation();
  const VarDecl *VD = Context.getVarDecl(Tok.getIdentifierInfo());
  if (!VD)
    return Diag.ReportError(Loc, "unknown type specifier");
  Lex();

  llvm::SmallVector<ExprResult, 4> ExprVec;
  if (Tok.is(tok::l_paren)) {
    // TODO: Parse "keyword =".
    do {
      Lex();
      ExprResult E = ParseExpression();
      if (E.isInvalid()) goto error;
      ExprVec.push_back(E);
    } while (Tok.is(tok::comma));

    if (!EatIfPresent(tok::r_paren)) {
      Diag.ReportError(Tok.getLocation(),
                       "expected ')' after type parameter specifier list");
      goto error;
    }
  }

  DS = new DerivedDeclSpec(new VarExpr(Loc, VD), ExprVec);
  return false;

 error:
  for (unsigned I = 0, N = ExprVec.size(); I != N; ++I)
    delete ExprVec[I].take();
  return true;
}

/// ParseDeclarationTypeSpec - Parse a declaration type spec construct.
/// 
///   [5.1] R502:
///     declaration-type-spec :=
///         intrinsic-type-spec
///      or TYPE ( derived-type-spec )
///      or CLASS ( derived-type-spec )
///      or CLASS ( * )
bool Parser::ParseDeclarationTypeSpec(DeclSpec *&DS) {
  // [4.4] R403:
  //   intrinsic-type-spec :=
  //       INTEGER [ kind-selector ]
  //    or REAL [ kind-selector ]
  //    or DOUBLE PRECISION
  //    or COMPLEX [ kind-selector ]
  //    or CHARACTER [ char-selector ]
  //    or LOGICAL [ kind-selector ]
  BuiltinType::TypeSpec TS;
  switch (Tok.getKind()) {
  default:                TS = BuiltinType::TS_Invalid;   break;
  case tok::kw_INTEGER:   TS = BuiltinType::TS_Integer;   break;
  case tok::kw_REAL:      TS = BuiltinType::TS_Real;      break;
  case tok::kw_COMPLEX:   TS = BuiltinType::TS_Complex;   break;
  case tok::kw_CHARACTER: TS = BuiltinType::TS_Character; break;
  case tok::kw_LOGICAL:   TS = BuiltinType::TS_Logical;   break;
  case tok::kw_DOUBLEPRECISION:
    TS = BuiltinType::TS_DoublePrecision;
    break;
  }

  switch (TS) {
  case BuiltinType::TS_Invalid:
    // We're parsing a TYPE or CLASS.
    break;
  default: {
    Lex();

    Selector Kind;
    if (EatIfPresent(tok::l_paren)) {
      if (ParseKindSelector(Kind))
        return true;

      if (!EatIfPresent(tok::r_paren))
        return Diag.ReportError(Tok.getLocation(),
                                "expected ')' after kind selector");
    }

    DS = new IntrinsicDeclSpec(Actions.ActOnBuiltinType(&Context, TS, Kind));
    return false;
  }
  case BuiltinType::TS_DoublePrecision: {
    Lex();
    if (Tok.is(tok::l_paren))
      return Diag.ReportError(Tok.getLocation(),
                             "'DOUBLE PRECISION' doesn't take a kind selector");
    Selector Kind;
    DS = new IntrinsicDeclSpec(Actions.ActOnBuiltinType(&Context, TS, Kind));
    return false;
  }
  case BuiltinType::TS_Character: {
    // [4.4.4.1] R424:
    //   char-selector :=
    //       length-selector
    //    or ( LEN = type-param-value , KIND = scalar-int-initialization-expr )
    //    or ( type-param-value , #
    //    #    [ KIND = ] scalar-int-initialization-expr )
    //    or ( KIND = scalar-int-initialization-expr [, LEN = type-param-value])
    //
    // [4.4.4.1] R425:
    //   length-selector :=
    //       ( [ LEN = ] type-param-value )
    //    or * char-length [,]
    //
    // [4.4.4.1] R426:
    //   char-length :=
    //       ( type-param-value )
    //    or scalar-int-literal-constant
    //
    // [4.2] R402:
    //   type-param-value :=
    //       scalar-int-expr
    //    or *
    //    or :
    Lex();

    Selector Len;
    Selector Kind;

    if (Tok.is(tok::star)) {
      Lex();
      ExprResult KindExpr = ParseExpression();
      Len.setKindExpr(KindExpr);
    } else {
      if (Tok.is(tok::l_paren)) {
        Lex(); // Eat '('.

        if (Tok.is(tok::kw_LEN)) {
          if (ParseLengthSelector(Len))
            return true;
        } else if (Tok.is(tok::kw_KIND)) {
          if (ParseKindSelector(Kind))
            return true;
        } else {
          ExprResult KindExpr = ParseExpression();
          Len.setKindExpr(KindExpr);
        }

        if (Tok.is(tok::comma)) {
          Lex(); // Eat ','.

          if (Tok.is(tok::kw_LEN)) {
            if (Len.getKindExpr().isInvalid())
              return Diag.ReportError(Tok.getLocation(),
                                      "multiple LEN selectors for this type");
            if (ParseLengthSelector(Len))
              return true;
          } else if (Tok.is(tok::kw_KIND)) {
            if (Kind.getKindExpr().isInvalid())
              return Diag.ReportError(Tok.getLocation(),
                                      "multiple KIND selectors for this type");
            if (ParseKindSelector(Kind))
              return true;
          } else {
            if (Kind.getKindExpr().isInvalid())
              return Diag.ReportError(Tok.getLocation(),
                                      "multiple KIND selectors for this type");

            ExprResult KindExpr = ParseExpression();
            Kind.setKindExpr(KindExpr);
          }
        }

        if (Tok.isNot(tok::r_paren))
          return Diag.ReportError(Tok.getLocation(),
                                  "expected ')' after selector");
        Lex();  // Eat ')'.
      }
    }

    DS = new IntrinsicDeclSpec(Actions.ActOnCharacterBuiltinType(&Context,
                                                                 Len,
                                                                 Kind));
    return false;
  }
  }

  if (EatIfPresent(tok::kw_TYPE)) {
    if (!EatIfPresent(tok::l_paren))
      return Diag.ReportError(Tok.getLocation(),
                              "expected '(' in type specification");

    if (ParseDerivedTypeSpec(DS))
      return true;

    if (!EatIfPresent(tok::r_paren))
      return Diag.ReportError(Tok.getLocation(),
                              "expected ')' in type specification");

    return Actions.ActOnTypeDeclSpec(&Context);
  }

  // TODO: Handle CLASS.

  return true;
}
