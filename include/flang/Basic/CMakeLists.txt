macro(flang_diag_gen component)
  flang_tablegen(Diagnostic${component}Kinds.inc
    -gen-flang-diags-defs -flang-component=${component}
    SOURCE Diagnostic.td
    TARGET FlangDiagnostic${component})
endmacro(flang_diag_gen)

flang_diag_gen(Common)
flang_diag_gen(Lex)
flang_diag_gen(Parse)
