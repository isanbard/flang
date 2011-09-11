//===-- DeclSpec.cpp - Fortran Declaration Type Specifier Interface ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The Fortran declaration type specifier interface.
//
//===----------------------------------------------------------------------===//

#include "flang/Sema/DeclSpec.h"
#include "flang/AST/Expr.h"
#include "flang/AST/Type.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/ErrorHandling.h"
using namespace flang;

namespace flang {
  namespace diag {
    enum {
#define DIAG(ENUM,FLAGS,DEFAULT_MAPPING,DESC,GROUP,\
             SFINAE,ACCESS,CATEGORY,BRIEF,FULL) ENUM,
#include "flang/Basic/DiagnosticParseKinds.inc"
#undef DIAG
      NUM_BUILTIN_PARSE_DIAGNOSTICS
    };
  }  // end namespace diag
}  // end namespace flang

DeclSpec::~DeclSpec() {}

template <class T>
static bool BadSpecifier(T TNew, T TPrev, const char *&PrevSpec,
                         unsigned &DiagID) {
  PrevSpec = DeclSpec::getSpecifierName(TPrev);
  DiagID = (TNew == TPrev ? diag::err_duplicate_declspec
            : diag::err_invalid_decl_spec_combination);
  return true;
}

const char *DeclSpec::getSpecifierName(DeclSpec::TST I) {
  switch (I) {
  case TST_unspecified:     return "unspecified";
  case TST_integer:         return "INTEGER";
  case TST_real:            return "REAL";
  case TST_doubleprecision: return "DOUBLEPRECISION";
  case TST_complex:         return "COMPLEX";
  case TST_character:       return "CHARACTER";
  case TST_logical:         return "LOGICAL";
  case TST_struct:          return "TYPE";
  }
  llvm_unreachable("Unknown typespec!");
}

const char *DeclSpec::getSpecifierName(DeclSpec::AS A) {
  switch (A) {
  case AS_unspecified:  return "unspecified";
  case AS_allocatable:  return "ALLOCATABLE";
  case AS_asynchronous: return "ASYNCHRONOUS";
  case AS_codimension:  return "CODIMENSION";
  case AS_contiguous:   return "CONTIGUOUS";
  case AS_dimension:    return "DIMENSION";
  case AS_external:     return "EXTERNAL";
  case AS_intrinsic:    return "INTRINSIC";
  case AS_optional:     return "OPTIONAL";
  case AS_parameter:    return "PARAMETER";
  case AS_pointer:      return "POINTER";
  case AS_protected:    return "PROTECTED";
  case AS_save:         return "SAVE";
  case AS_target:       return "TARGET";
  case AS_value:        return "VALUE";
  case AS_volatile:     return "VOLATILE";
  }
  llvm_unreachable("Unknown typespec!");
}

const char *DeclSpec::getSpecifierName(DeclSpec::IS I) {
  switch (I) {
  case IS_unspecified: return "unspecified";
  case IS_in:          return "IN";
  case IS_out:         return "OUT";
  case IS_inout:       return "INOUT";
  }
  llvm_unreachable("Unknown typespec!");
}

void DeclSpec::setDimensions(ArrayRef<ExprResult> Dims) {
  assert(hasAttributeSpec(AS_dimension) &&
         "Adding dimensions to a non-array declspec!");
  Dimensions.reserve(Dims.size());
  for (ArrayRef<ExprResult>::iterator
         I = Dims.begin(), E = Dims.end(); I != E; ++I)
    Dimensions.push_back(*I);
}
