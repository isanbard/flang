//==--- DiagnosticCommonKinds.td - common diagnostics ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Common Helpers
//===----------------------------------------------------------------------===//

let Component = "Common" in {

// Basic.

def fatal_too_many_errors
  : Error<"too many errors emitted, stopping now">, DefaultFatal; 

def note_previous_definition : Note<"previous definition is here">;
def note_previous_declaration : Note<"previous declaration is here">;

// Source manager
def err_cannot_open_file : Error<"cannot open file '%0': %1">, DefaultFatal;
def err_file_modified : Error<
  "file '%0' modified since it was first processed">, DefaultFatal;

}
