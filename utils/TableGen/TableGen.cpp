//===- TableGen.cpp - Top-Level TableGen implementation for Flang ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the main function for Flang's TableGen.
//
//===----------------------------------------------------------------------===//

#include "TableGenBackends.h" // Declares all backends.
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Main.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace flang;

enum ActionType {
  GenFlangDiagsDefs,
  GenFlangDiagGroups,
  GenFlangDiagsIndexName,
  GenFlangDeclNodes,
  GenFlangStmtNodes
};

namespace {
  cl::opt<ActionType>
  Action(cl::desc("Action to perform:"),
         cl::values(clEnumValN(GenFlangDiagsDefs, "gen-flang-diags-defs",
                               "Generate Flang diagnostics definitions"),
                    clEnumValN(GenFlangDiagGroups, "gen-flang-diag-groups",
                               "Generate Flang diagnostic groups"),
                    clEnumValN(GenFlangDiagsIndexName,
                               "gen-flang-diags-index-name",
                               "Generate Flang diagnostic name index"),
                    clEnumValN(GenFlangDeclNodes, "gen-flang-decl-nodes",
                               "Generate Flang AST declaration nodes"),
                    clEnumValN(GenFlangStmtNodes, "gen-flang-stmt-nodes",
                               "Generate Flang AST statement nodes"),
                    clEnumValEnd));

  cl::opt<std::string>
  FlangComponent("flang-component",
                 cl::desc("Only use warnings from specified component"),
                 cl::value_desc("component"), cl::Hidden);

bool FlangTableGenMain(raw_ostream &OS, RecordKeeper &Records) {
  switch (Action) {
  case GenFlangDiagsDefs:
    EmitFlangDiagsDefs(Records, OS, FlangComponent);
    break;
  case GenFlangDiagGroups:
    EmitFlangDiagGroups(Records, OS);
    break;
  case GenFlangDiagsIndexName:
    EmitFlangDiagsIndexName(Records, OS);
    break;
  case GenFlangDeclNodes:
    EmitFlangASTNodes(Records, OS, "Decl", "Decl");
    EmitFlangDeclContext(Records, OS);
    break;
  case GenFlangStmtNodes:
    EmitFlangASTNodes(Records, OS, "Stmt", "");
    break;
  }

  return false;
}
}

int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);
  cl::ParseCommandLineOptions(argc, argv);

  return TableGenMain(argv[0], &FlangTableGenMain);
}
