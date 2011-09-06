//===--- Specifiers.h - Declaration and Type Specifiers ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines various enumerations that describe declaration and
// type specifiers.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_BASIC_SPECIFIERS_H
#define FLANG_BASIC_SPECIFIERS_H

namespace flang {
  /// \brief [R404] Specifies the intrinsic type specifier.
  enum IntrinsicTypeSpec {
    ITS_unspecified,
    ITS_integer,
    ITS_real,
    ITS_doubleprecision,
    ITS_complex,
    ITS_character,
    ITS_logical
  };

  /// \brief [R502] Specifies the attribute specifiers for types.
  enum AttributeSpecifier {
    AS_unspecified     = 0,
    AS_asynchronous    = 1 << 1,
    AS_codimension     = 1 << 2,
    AS_contiguous      = 1 << 3,
    AS_dimension       = 1 << 4,
    AS_external        = 1 << 5,
    AS_intrinsic       = 1 << 6,
    AS_optional        = 1 << 7,
    AS_pointer         = 1 << 8,
    AS_protected       = 1 << 9,
    AS_save            = 1 << 10,
    AS_target          = 1 << 11,
    AS_value           = 1 << 12
  };

  /// \brief [R523] Specifies the intent specifier.
  enum IntentSpecifier {
    IS_unspecified,
    IS_in,
    IS_out,
    IS_inout
  };

  /// \brief [R507] Access specifier (public, private).
  enum AccessSpecifier {
    AC_unspecified,
    AC_public,
    AC_private
  };

} // end namespace flang

#endif // FLANG_BASIC_SPECIFIERS_H
