//===- TableGenBackends.h - Declarations for Flang TableGen Backends ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations for all of the Flang TableGen
// backends. A "TableGen backend" is just a function. See
// "$LLVM_ROOT/utils/TableGen/TableGenBackends.h" for more info.
//
//===----------------------------------------------------------------------===//

#include <string>

namespace llvm {
  class raw_ostream;
  class RecordKeeper;
}

using llvm::raw_ostream;
using llvm::RecordKeeper;

namespace flang {

void EmitFlangDeclContext(RecordKeeper &RK, raw_ostream &OS);
void EmitFlangASTNodes(RecordKeeper &RK, raw_ostream &OS,
                       const std::string &N, const std::string &S);

void EmitFlangDiagsDefs(RecordKeeper &Records, raw_ostream &OS,
                        const std::string &Component);
void EmitFlangDiagGroups(RecordKeeper &Records, raw_ostream &OS);
void EmitFlangDiagsIndexName(RecordKeeper &Records, raw_ostream &OS);

} // end namespace flang
