FLANG_LEVEL := ../../..
TD_SRC_DIR = $(PROJ_SRC_DIR)/../Basic
BUILT_SOURCES = DeclNodes.inc

TABLEGEN_INC_FILES_COMMON = 1

include $(FLANG_LEVEL)/Makefile

$(ObjDir)/DeclNodes.inc.tmp : $(TD_SRC_DIR)/DeclNodes.td $(TBLGEN) \
                              $(ObjDir)/.dir
	$(Echo) "Building Flang declaration node tables with tblgen"
	$(Verb) $(FlangTableGen) -gen-flang-decl-nodes -o $(call SYSPATH, $@) $<
