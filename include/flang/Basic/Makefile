FLANG_LEVEL := ../../..
BUILT_SOURCES = \
	DiagnosticCommonKinds.inc DiagnosticParseKinds.inc

TABLEGEN_INC_FILES_COMMON = 1

include $(FLANG_LEVEL)/Makefile

INPUT_TDS = $(wildcard $(PROJ_SRC_DIR)/Diagnostic*.td)

$(ObjDir)/Diagnostic%Kinds.inc.tmp : Diagnostic.td Diagnostic%Kinds.td $(TBLGEN) $(ObjDir)/.dir
	$(Echo) "Building Flang $(patsubst Diagnostic%Kinds.inc.tmp,%,$(@F)) diagnostic tables with tblgen"
	$(Verb) $(TableGen) -gen-clang-diags-defs -clang-component=$(patsubst Diagnostic%Kinds.inc.tmp,%,$(@F)) -o $(call SYSPATH, $@) $<
