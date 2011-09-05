##===- tools/driver/Makefile -------------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

FLANG_LEVEL := ../..

TOOLNAME = flang

# We don't currently expect production Flang builds to be interested in
# plugins. This is important for startup performance.
ifdef FLANG_IS_PRODUCTION
TOOL_NO_EXPORTS := 1
endif

ifdef FLANG_ORDER_FILE
TOOL_ORDER_FILE := $(FLANG_ORDER_FILE)
endif

# Include tool version information on OS X.
TOOL_INFO_PLIST := Info.plist

# Include this here so we can get the configuration of the targets that have
# been configured for construction. We have to do this early so we can set up
# LINK_COMPONENTS before including Makefile.rules
include $(FLANG_LEVEL)/../../Makefile.config

LINK_COMPONENTS := $(TARGETS_TO_BUILD) asmparser bitreader bitwriter codegen \
                   ipo selectiondag
USEDLIBS = flangAST.a flangFrontend.a flangParse.a flangSema.a flangBasic.a

include $(FLANG_LEVEL)/Makefile

# Set the tool version information values.
ifeq ($(HOST_OS),Darwin)
ifdef FLANG_VENDOR
TOOL_INFO_NAME := $(FLANG_VENDOR) flang
else
TOOL_INFO_NAME := flang
endif

ifdef FLANG_VENDOR_UTI
TOOL_INFO_UTI := $(FLANG_VENDOR_UTI)
else
TOOL_INFO_UTI := org.llvm.flang
endif

#TOOL_INFO_VERSION := $(word 3,$(shell grep "FLANG_VERSION " \
#	$(PROJ_OBJ_DIR)/$(FLANG_LEVEL)/include/flang/Basic/Version.inc))

ifdef LLVM_SUBMIT_VERSION
TOOL_INFO_BUILD_VERSION := $(LLVM_SUBMIT_VERSION).$(LLVM_SUBMIT_SUBVERSION)
else
TOOL_INFO_BUILD_VERSION := 
endif
endif

# Translate make variable to define when building a "production" Cling.
ifdef FLANG_IS_PRODUCTION
CPP.Defines += -DFLANG_IS_PRODUCTION
endif
