//==--- DiagnosticParseKinds.td - libparse diagnostics --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Parser Diagnostics
//===----------------------------------------------------------------------===//

let Component = "Parse" in {

let CategoryName = "Parse Issue" in {

// Generic errors
def err_parse_error : Error<"parse error">;
def err_duplicate_declspec : Error<"duplicate '%0' declaration specifier">;

// Declarations
def err_invalid_decl_spec_combination : Error<
  "cannot combine with previous '%0' declaration specifier">;

} // end of Parse Issue category.
} // end of Parser diagnostics
