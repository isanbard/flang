set(LLVM_LINK_COMPONENTS support)

add_flang_library(flangAST
	ASTContext.cpp
	Decl.cpp
	DeclGroup.cpp
	DeclarationName.cpp
	Expr.cpp
	FormatSpec.cpp
	Stmt.cpp
	StmtDumper.cpp
	Type.cpp
  )

add_dependencies(flangAST
  FlangDiagnosticCommon
  FlangDeclNodes
  )

target_link_libraries(flangAST
  flangBasic
  )
