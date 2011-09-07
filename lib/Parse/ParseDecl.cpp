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
#include "flang/Sema/Sema.h"
using namespace flang;

bool Parser::AssignTypeQual(DeclSpec &DS, DeclSpec::TQ Val) {
  if (DS.hasTypeQual(Val))
    return Diag.ReportError(Tok.getLocation(),
                            "type qualifier defined more than once");
  DS.setTypeQual(Val);
  Lex();
  return false;
}

/// AssignAttrSpec - Helper function that assigns the attribute specification to
/// the list, but reports an error if that attribute was all ready assigned.
bool Parser::AssignAttrSpec(DeclSpec &DS, DeclSpec::AS Val) {
  if (DS.hasAttributeSpec(Val))
    return Diag.ReportError(Tok.getLocation(),
                            "attribute specification defined more than once");
  DS.setAttributeSpec(Val);
  Lex();
  return false;
}

/// AssignAccessSpec - Helper function that assigns the access specification to
/// the DeclSpec, but reports an error if that access spec was all ready
/// assigned.
bool Parser::AssignAccessSpec(DeclSpec &DS, DeclSpec::AC Val) {
  if (DS.hasAccessSpec(Val))
    return Diag.ReportError(Tok.getLocation(),
                            "access specification defined more than once");
  DS.setAccessSpec(Val);
  Lex();
  return false;
}

/// AssignIntentSpec - Helper function that assigns the intent specification to
/// the DeclSpec, but reports an error if that intent spec was all ready
/// assigned.
bool Parser::AssignIntentSpec(DeclSpec &DS, DeclSpec::IS Val) {
  if (DS.hasIntentSpec(Val))
    return Diag.ReportError(Tok.getLocation(),
                            "intent specification defined more than once");
  DS.setIntentSpec(Val);
  Lex();
  return false;
}

bool Parser::ParseTypeDeclarationList(DeclSpec &DS,
                                      SmallVectorImpl<DeclResult> &Decls) {
  while (!Tok.isAtStartOfStatement()) {
    llvm::SMLoc IDLoc = Tok.getLocation();
    const IdentifierInfo *ID = Tok.getIdentifierInfo();
    if (!ID)
      return Diag.ReportError(IDLoc,
                              "expected an identifier in TYPE list");
    Lex();

    // FIXME: If there's a '(' here, it might be parsing an array decl.

    Decls.push_back(Actions.ActOnEntityDecl(Context, DS, IDLoc, ID));

    llvm::SMLoc CommaLoc = Tok.getLocation();
    if (!EatIfPresent(tok::comma)) {
      if (!Tok.isAtStartOfStatement())
        return Diag.ReportError(Tok.getLocation(),
                                "expected a ',' in TYPE list");

      break;
    }

    if (Tok.isAtStartOfStatement())
      return Diag.ReportError(CommaLoc,
                              "expected an identifier after ',' in TYPE list");
  }

  return false;
}

/// ParseTypeDeclarationStmt - Parse a type-declaration-stmt construct.
///
///   [5.2.1] R501:
///     type-declaration-stmt :=
///         declaration-type-spec [ [ , attr-spec ] ... :: ] entity-decl-list
bool Parser::ParseTypeDeclarationStmt(SmallVectorImpl<DeclResult> &Decls) {
  if (!Tok.isAtStartOfStatement())
    return true;
  
  DeclSpec DS;
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
    //    or language-binding-spec // TODO!
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
      if (AssignAttrSpec(DS, DeclSpec::AS_allocatable))
        goto error;
      break;
    case tok::kw_ASYNCHRONOUS:
      if (AssignAttrSpec(DS, DeclSpec::AS_asynchronous))
        goto error;
      break;
    case tok::kw_CODIMENSION:
      if (AssignAttrSpec(DS, DeclSpec::AS_codimension))
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
      if (AssignAttrSpec(DS, DeclSpec::AS_contiguous))
        goto error;
      break;
    case tok::kw_DIMENSION:
      if (AssignAttrSpec(DS, DeclSpec::AS_dimension))
        goto error;
      if (ParseArraySpec(Dimensions))
        goto error;
      break;
    case tok::kw_EXTERNAL:
      if (AssignAttrSpec(DS, DeclSpec::AS_external))
        goto error;
      break;
    case tok::kw_INTENT:
      Lex();
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
        if (AssignIntentSpec(DS, DeclSpec::IS_in))
          goto error;
        break;
      case tok::kw_OUT:
        if (AssignIntentSpec(DS, DeclSpec::IS_out))
          goto error;
        break;
      case tok::kw_INOUT:
        if (AssignIntentSpec(DS, DeclSpec::IS_inout))
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
      if (AssignAttrSpec(DS, DeclSpec::AS_intrinsic))
        goto error;
      break;
    case tok::kw_OPTIONAL:
      if (AssignAttrSpec(DS, DeclSpec::AS_optional))
        goto error;
      break;
    case tok::kw_PARAMETER:
      if (AssignAttrSpec(DS, DeclSpec::AS_parameter))
        goto error;
      break;
    case tok::kw_POINTER:
      if (AssignAttrSpec(DS, DeclSpec::AS_pointer))
        goto error;
      break;
    case tok::kw_PROTECTED:
      if (AssignAttrSpec(DS, DeclSpec::AS_protected))
        goto error;
      break;
    case tok::kw_SAVE:
      if (AssignAttrSpec(DS, DeclSpec::AS_save))
        goto error;
      break;
    case tok::kw_TARGET:
      if (AssignAttrSpec(DS, DeclSpec::AS_target))
        goto error;
      break;
    case tok::kw_VALUE:
      if (AssignAttrSpec(DS, DeclSpec::AS_value))
        goto error;
      break;
    case tok::kw_VOLATILE:
      if (AssignAttrSpec(DS, DeclSpec::AS_volatile))
        goto error;
      break;

    // Access Control Specifiers
    case tok::kw_PUBLIC:
      if (AssignAccessSpec(DS, DeclSpec::AC_public))
        goto error;
      break;
    case tok::kw_PRIVATE:
      if (AssignAccessSpec(DS, DeclSpec::AC_private))
        goto error;
      break;
    }
  }

  EatIfPresent(tok::coloncolon);

  if (Tok.isAtStartOfStatement()) {
    // A type without any identifiers.
    Diag.ReportError(Tok.getLocation(),
                     "expected an identifier in TYPE list");
    goto error;
  }

  if (ParseTypeDeclarationList(DS, Decls))
    goto error;

  return false;
 error:
  for (llvm::SmallVectorImpl<ExprResult>::iterator
         I = Dimensions.begin(), E = Dimensions.end(); I != E; ++I)
    delete I->take();
  return true;
}

/// Parse the optional KIND or LEN selector.
/// 
///   [R405]:
///     kind-selector :=
///         ( [ KIND = ] scalar-int-initialization-expr )
///   [R425]:
///     length-selector :=
///         ( [ LEN = ] type-param-value )
ExprResult Parser::ParseSelector(bool IsKindSel) {
  if (EatIfPresent(IsKindSel ? tok::kw_KIND : tok::kw_LEN)) {
    if (!EatIfPresent(tok::equal)) {
      if (Tok.isNot(tok::l_paren))
        return Diag.ReportError(Tok.getLocation(),
                                IsKindSel ? 
                                "invalid kind selector" :
                                "invalid length selector");

      // TODO: We have a "REAL (KIND(10D0)) :: x" situation.
      return false;
    }
  }

  return ParseExpression();
}

/// ParseDerivedTypeSpec - Parse the type declaration specifier.
///
///   [R455]:
///     derived-type-spec :=
///         type-name [ ( type-param-spec-list ) ]
///
///   [R456]:
///     type-param-spec :=
///         [ keyword = ] type-param-value
bool Parser::ParseDerivedTypeSpec(DeclSpec &DS) {
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

  //  DS = new DerivedDeclSpec(new VarExpr(Loc, VD), ExprVec);
  return false;

 error:
  for (unsigned I = 0, N = ExprVec.size(); I != N; ++I)
    delete ExprVec[I].take();
  return true;
}

/// ParseTypeOrClassDeclTypeSpec - Parse a TYPE(...) or CLASS(...) declaration
/// type spec.
/// 
///   [R502]:
///     declaration-type-spec :=
///         TYPE ( derived-type-spec )
///      or CLASS ( derived-type-spec )
///      or CLASS ( * )
bool Parser::ParseTypeOrClassDeclTypeSpec(DeclSpec &DS) {
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

  return false;
}

/// ParseDeclarationTypeSpec - Parse a declaration type spec construct.
/// 
///   [R502]:
///     declaration-type-spec :=
///         intrinsic-type-spec
///      or TYPE ( derived-type-spec )
///      or CLASS ( derived-type-spec )
///      or CLASS ( * )
bool Parser::ParseDeclarationTypeSpec(DeclSpec &DS) {
  // [R403]:
  //   intrinsic-type-spec :=
  //       INTEGER [ kind-selector ]
  //    or REAL [ kind-selector ]
  //    or DOUBLE PRECISION
  //    or COMPLEX [ kind-selector ]
  //    or CHARACTER [ char-selector ]
  //    or LOGICAL [ kind-selector ]
  switch (Tok.getKind()) {
  default:
    DS.SetIntrinsicTypeSpec(DeclSpec::ITS_unspecified);
    break;
  case tok::kw_INTEGER:
    DS.SetIntrinsicTypeSpec(DeclSpec::ITS_integer);
    break;
  case tok::kw_REAL:
    DS.SetIntrinsicTypeSpec(DeclSpec::ITS_real);
    break;
  case tok::kw_COMPLEX:
    DS.SetIntrinsicTypeSpec(DeclSpec::ITS_complex);
    break;
  case tok::kw_CHARACTER:
    DS.SetIntrinsicTypeSpec(DeclSpec::ITS_character);
    break;
  case tok::kw_LOGICAL:
    DS.SetIntrinsicTypeSpec(DeclSpec::ITS_logical);
    break;
  case tok::kw_DOUBLEPRECISION:
    DS.SetIntrinsicTypeSpec(DeclSpec::ITS_doubleprecision);
    break;
  }

  if (DS.getIntrinsicTypeSpec() == DeclSpec::ITS_unspecified)
    if (ParseTypeOrClassDeclTypeSpec(DS))
      return true;

  ExprResult Kind;
  ExprResult Len;

  switch (DS.getIntrinsicTypeSpec()) {
  default:
    Lex();

    if (EatIfPresent(tok::l_paren)) {
      Kind = ParseSelector(true);
      if (Kind.isInvalid())
        return true;

      if (!EatIfPresent(tok::r_paren))
        return Diag.ReportError(Tok.getLocation(),
                                "expected ')' after kind selector");
    }

    break;
  case DeclSpec::ITS_doubleprecision:
    Lex();
    if (Tok.is(tok::l_paren))
      return Diag.ReportError(Tok.getLocation(),
                             "'DOUBLE PRECISION' doesn't take a kind selector");
    break;
  case DeclSpec::ITS_character:
    // [R424]:
    //   char-selector :=
    //       length-selector
    //    or ( LEN = type-param-value , KIND = scalar-int-initialization-expr )
    //    or ( type-param-value , #
    //    #    [ KIND = ] scalar-int-initialization-expr )
    //    or ( KIND = scalar-int-initialization-expr [, LEN = type-param-value])
    //
    // [R425]:
    //   length-selector :=
    //       ( [ LEN = ] type-param-value )
    //    or * char-length [,]
    //
    // [R426]:
    //   char-length :=
    //       ( type-param-value )
    //    or scalar-int-literal-constant
    //
    // [R402]:
    //   type-param-value :=
    //       scalar-int-expr
    //    or *
    //    or :
    Lex();

    if (Tok.is(tok::star)) {
      Lex();
      ExprResult KindExpr = ParseExpression();
      Len = KindExpr;
    } else {
      if (Tok.is(tok::l_paren)) {
        Lex(); // Eat '('.

        if (Tok.is(tok::kw_LEN)) {
          Len = ParseSelector(false);
          if (Len.isInvalid())
            return true;
        } else if (Tok.is(tok::kw_KIND)) {
          Kind = ParseSelector(true);
          if (Kind.isInvalid())
            return true;
        } else {
          ExprResult KindExpr = ParseExpression();
          Len = KindExpr;
        }

        if (Tok.is(tok::comma)) {
          Lex(); // Eat ','.

          if (Tok.is(tok::kw_LEN)) {
            if (Len.isInvalid())
              return Diag.ReportError(Tok.getLocation(),
                                      "multiple LEN selectors for this type");
            Len = ParseSelector(false);
            if (Len.isInvalid())
              return true;
          } else if (Tok.is(tok::kw_KIND)) {
            if (Kind.isInvalid())
              return Diag.ReportError(Tok.getLocation(),
                                      "multiple KIND selectors for this type");
            Kind = ParseSelector(true);
            if (Kind.isInvalid())
              return true;
          } else {
            if (Kind.isInvalid())
              return Diag.ReportError(Tok.getLocation(),
                                      "multiple KIND selectors for this type");

            ExprResult KindExpr = ParseExpression();
            Kind = KindExpr;
          }
        }

        if (!EatIfPresent(tok::r_paren))
          return Diag.ReportError(Tok.getLocation(),
                                  "expected ')' after selector");
      }
    }

    break;
  }

  // Set the selectors for declspec.
  DS.setKindSelector(Kind.get());
  DS.setLengthSelector(Len.get());
  return true;
}
