set(LLVM_LINK_COMPONENTS mc)

add_flang_library(flangBasic
  Diagnostic.cpp
  IdentifierTable.cpp
  Token.cpp
  TokenKinds.cpp
)

# Determine Subversion revision.
# FIXME: This only gets updated when CMake is run, so this revision number
# may be out-of-date!
if( NOT IS_SYMLINK "${FLANG_SOURCE_DIR}" )  # See PR 8437
  find_package(Subversion)
endif()
if (Subversion_FOUND AND EXISTS "${FLANG_SOURCE_DIR}/.svn")
  Subversion_WC_INFO(${FLANG_SOURCE_DIR} FLANG)
  set_source_files_properties(Version.cpp
    PROPERTIES COMPILE_DEFINITIONS "SVN_REVISION=\"${FLANG_WC_REVISION}\"")
endif()

add_dependencies(flangBasic
  FlangDiagnosticCommon
  FlangDiagnosticLex
  FlangDiagnosticParse
  )  
