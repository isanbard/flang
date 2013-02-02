set(LLVM_LINK_COMPONENTS Support)

add_tablegen(flang-tblgen FLANG
  FlangASTNodesEmitter.cpp
  FlangDiagnosticsEmitter.cpp
  TableGen.cpp
  )
