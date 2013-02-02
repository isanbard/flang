add_flang_library(flangParse
  Lexer.cpp
  ParseDecl.cpp
  ParseExec.cpp
  ParseExpr.cpp
  Parser.cpp
)

add_dependencies(flangParse
  FlangDeclNodes
  FlangDiagnosticParse
  )
