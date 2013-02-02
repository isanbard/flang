set( LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  asmparser
  bitreader
  bitwriter
  codegen
  ipo
  selectiondag
  )

add_flang_executable(flang
  Main.cpp
  )

target_link_libraries(flang
  flangAST
  flangFrontend
  flangParse
  flangSema
  flangBasic
  )

set_target_properties(flang PROPERTIES VERSION ${FLANG_EXECUTABLE_VERSION})

add_dependencies(flang flang-headers)

if(UNIX)
  set(flang_binary "flang${CMAKE_EXECUTABLE_SUFFIX}")
else()
  set(flang_binary "${LLVM_BINARY_DIR}/bin/${CMAKE_CFG_INTDIR}/flang${CMAKE_EXECUTABLE_SUFFIX}")
endif()

set_property(DIRECTORY APPEND
  PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${flang_pp})

install(TARGETS flang
  RUNTIME DESTINATION bin)

