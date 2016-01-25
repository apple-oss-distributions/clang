##
# Clang B&I Build Logic
##

# This makefile defines the common build logic which is shared by all the Clang
# projects. Projects are expected to define the following variables and then
# include this Makefile::
#
#   Clang_Use_Assertions := {0, 1}
#     Enable/disable asserts.
#
#   Clang_Build_All := {0, 1}
#     Enable the building of all LLVM tools, for use in testing.
#
#   Clang_Extra_Options := ...
#     Additional options to pass to make.
#
#   Clang_Optimize_Option := ...
#     The optimization flags to use.
#
#   Clang_Linker_Options := ...
#     The linker flags to use.

# Particular build targets (which correspond to project configurations) may
# override various defaults based on what makes sense to build/install for that
# particular target.

##
# Default target.

all: help

help:
	@echo "usage: make [{VARIABLE=VALUE}*] <target>"
	@echo
	@echo "The Apple Clang makefile is primarily intended for use with XBS."
	@echo
	@echo "Supported B&I related targets are:"
	@echo "  installsrc    -- Copy source files from the current" \
	      "directory to the SRCROOT."
	@echo "  clean         -- Does nothing, just for XBS support."
	@echo "  installhdrs   -- Does nothing, just for XBS support."
	@echo "  install       -- Alias for install-clang."
	@echo "  install-clang -- Build the Apple Clang compiler."
	@echo "  install-cross -- Build the Apple Clang compiler, for ARM."
	@echo "  install-clang-links"
	@echo "                -- Install links from a platforms subdirectory" \
	      "to the root clang."
	@echo
	@echo "The Makefile also supports the following utility targets:"
	@echo "  commit-buildbot-order-file -- order file update utility"
	@echo "    This command is for use with buildbot, it copies back a"
	@echo "    generated order file to the "

##
# Variable defaults.

# Install to $TOOLCHAIN_INSTALL_DIR if it is set, otherwise $DEVELOPER_DIR.
DEVELOPER_DIR ?= /Developer
DT_TOOLCHAIN_DIR ?= $(DEVELOPER_DIR)
TOOLCHAIN_INSTALL_DIR ?= $(DT_TOOLCHAIN_DIR)
Default_Install_Root := /$(DT_VARIANT)$(TOOLCHAIN_INSTALL_DIR)
# Don't install root links or license.
Post_Install_RootLinks := 0
Post_Install_OpenSourceLicense := 0
# Install the magic file that enables use of -fobjc-gc for Apple-internal use.
Post_Install_EnableObjCGC := 1
# Install to .../usr
Install_Path_Suffix := usr
# Include x86, ARM, and ARM64 backends.
LLVM_Backends := x86,arm,arm64
# Don't use extra make variables.
Extra_Make_Variables :=
# Don't install any archive files.
Extra_Make_Variables += NO_INSTALL_ARCHIVES=1
# LLVM level install target is 'install-clang.
LLVM_Install_Target := \
	OPTIONAL_DIRS="tools/llvm-cov tools/dsymutil tools/llvm-profdata tools/llvm-dwarfdump" \
	install-clang

##
# Per Project/Target Configuration

ifeq ($(MAKECMDGOALS),install-cross)

# Install to / by default.
Default_Install_Root := /
# Don't build compiler-rt.
Extra_Make_Variables += CLANG_NO_RUNTIME=1
# Never bootstrap.
Clang_Enable_Bootstrap := 0

else ifeq ($(MAKECMDGOALS),install-clang-links)

# Dummy project which only installs compiler links from the INSTALL_LOCATION to
# the primary SDK compiler.

else

# Default project (clang).

# Install root links and license when no install location is set.
ifeq ($(INSTALL_LOCATION),)
Post_Install_OpenSourceLicense := 1
endif

endif

##
# B&I Build Logic
##

# Require Clang_Version to be set.
ifeq ($(Clang_Version),)
$(error "invalid setting for clang version: '$(Clang_Version)'")
endif

# Set RC_ProjectSourceVersion, if unspecified, and a real target (i.e. not one
# of installsrc, clean, help, the default target, or any utility target).
ifeq ($(RC_ProjectSourceVersion),)
ifeq ($(MAKECMDGOALS),installsrc)
else ifeq ($(MAKECMDGOALS),clean)
else ifeq ($(MAKECMDGOALS),help)
else ifeq ($(MAKECMDGOALS),)
else ifeq ($(MAKECMDGOALS),commit-buildbot-order-file)
else
$(error "B&I build variable RC_ProjectSourceVersion must be set")
endif
endif

# Set the Clang_Tag based on RC_ProjectSourceVersion.
Clang_Tag := "clang-$(RC_ProjectSourceVersion)"

# Select optimized mode.
ifeq ($(Clang_Use_Optimized), 1)
Optimized_Configure_Flag :=  --enable-optimized
Build_Mode := Release
else ifeq ($(Clang_Use_Optimized), 0)
Optimized_Configure_Flag :=  --disable-optimized
Build_Mode := Debug
else
$(error "invalid setting for clang optimized: '$(Clang_Use_Optimized)'")
endif

# Select assertions mode.
ifeq ($(Clang_Use_Assertions), 1)
Assertions_Configure_Flag :=  --enable-assertions
Build_Mode := $(Build_Mode)+Asserts
else ifeq ($(Clang_Use_Assertions), 0)
Assertions_Configure_Flag :=  --disable-assertions
else
$(error "invalid setting for clang assertions: '$(Clang_Use_Assertions)'")
endif

# Select final build target based on automatic order file generation.
ifeq ($(Clang_Autogenerate_Order_File), 1)
Final_Build_Target := build-clang_final_ordered
else ifeq ($(Clang_Autogenerate_Order_File), 0)
Final_Build_Target := build-clang_final
else
$(error "invalid setting for clang autogenerate order file: '$(Clang_Autogenerate_Order_File)'")
endif

# Select whether to regenerate profile data.
ifeq ($(Clang_Autogenerate_Profile), 1)
$(OBJROOT)/clang.profdata: generate-clang-profdata
else ifeq ($(Clang_Autogenerate_Profile), 0)
else
$(error "invalid setting for clang autogenerate profile: '$(Clang_Autogenerate_Profile)'")
endif

# Select whether to build everything (for testing purposes).
Clang_Only_Build_Target := ONLY_TOOLS="clang lto llvm-cov dsymutil llvm-profdata llvm-dwarfdump" all
ifeq ($(Clang_Build_All), 1)
Clang_Build_Target := all
else ifeq ($(Clang_Build_All), 0)
Clang_Build_Target := $(Clang_Only_Build_Target)
else
$(error "invalid setting for clang build all mode: '$(Clang_Build_All)'")
endif

# Set makefile variables to pass during build and install.
Clang_Make_Variables := $(Extra_Make_Variables) KEEP_SYMBOLS=1 VERBOSE=1 \
                        CLANG_VENDOR=Apple \
                        CLANG_VENDOR_UTI=com.apple.compilers.llvm.clang \
                        BUILD_CLANG_ONLY=$(Clang_Build_No_Tools)
Clang_Make_Variables += CLANG_VERSION=$(Clang_Version)

# If CLANG_REPOSITORY_STRING has been set via an argument to buildit, use that
# (and expect an SVN_REVISION to have been provided), otherwise we use the tag
# name.
ifdef CLANG_REPOSITORY_STRING
ifndef SVN_REVISION
$(error "not setting for SVN_REVISION (required with CLANG_REPOSITORY_STRING)")
endif
Clang_Make_Variables += CLANG_REPOSITORY_STRING=$(CLANG_REPOSITORY_STRING)
Clang_Make_Variables += SVN_REVISION=$(SVN_REVISION)
else
Clang_Make_Variables += CLANG_REPOSITORY_STRING=$(Clang_Tag)
endif

# Set LLVM_VERSION_INFO make variable. We do this here because setting it in the
# CC options for configure ends up breaking tests that can't be bothered to
# quote things properly, and that is too hard to fix.
Clang_Make_Variables += \
  LLVM_VERSION_INFO="Apple LLVM $(Clang_Version) (clang-$(RC_ProjectSourceVersion))"

# Set destination information.
ifneq ($(INSTALL_LOCATION),)
Install_Root := $(INSTALL_LOCATION)
else ifneq ($(Default_Install_Root),)
Install_Root := $(Default_Install_Root)
else
$(error "invalid setting for default install root: '$(Default_Install_Root)'")
endif

# Set Install_Prefix.
Install_Prefix := $(Install_Root)

ifneq ($(Install_Path_Suffix),)
Install_Prefix := $(Install_Prefix)/$(Install_Path_Suffix)
else
$(error "invalid setting for install path suffix: '$(Install_Path_Suffix)'")
endif

# Set build version and subversion, for embedding into dylibs.
SourceVersion := $(shell echo $(RC_ProjectSourceVersion) | sed -e 's/\([0-9]*\).*/\1/')
SourceSubversion := $(shell echo $(RC_ProjectSourceVersion) | sed -e 's/[^.]*\.\([0-9]*\)/\1/')
ifneq ($(SourceVersion),)
Clang_Make_Variables += LLVM_SUBMIT_VERSION=$(SourceVersion)
ifneq ($(SourceVersion),$(RC_ProjectSourceVersion))
Clang_Make_Variables += LLVM_SUBMIT_SUBVERSION=$(SourceSubversion)
else
Clang_Make_Variables += LLVM_SUBMIT_SUBVERSION=
endif
endif

# When building libLTO as part of Clang, we need to offset the version to keep
# the sequencing monotonic.
Clang_Make_Variables += LLVM_LTO_VERSION_OFFSET=3000

# Set extra compile options.
Extra_Options := $(Clang_Extra_Options)
Final_Extra_Options := $(Extra_Options) $(Clang_Final_Extra_Options)

# Enable LTO if requested.
ifeq ($(Clang_Enable_LTO),1)
Final_Extra_Options += -flto -gline-tables-only
else ifeq ($(Clang_Enable_LTO),0)
Final_Extra_Options += -g
else
$(error "invalid setting for Clang_Enable_LTO: '$(Clang_Enable_LTO)'")
endif

# Enable PGO if requested. Ignore this for no-bootstrap builds, since the
# system compiler may not support PGO or handle the latest profile format.
ifeq ($(Clang_Enable_PGO),1)
ifeq ($(Clang_Enable_Bootstrap), 1)
Final_Extra_Options += -fprofile-instr-use=$(OBJROOT)/clang.profdata
Final_Extra_Options += -Wno-profile-instr-unprofiled
configure-clang_stage2: $(OBJROOT)/clang.profdata
configure-cross: $(OBJROOT)/clang.profdata
endif
endif

# Set configure flags.
Common_Configure_Flags = \
		  --enable-targets=$(LLVM_Backends) \
		  --disable-timestamps \
		  $(Assertions_Configure_Flag) \
		  $(Optimized_Configure_Flag) \
                  --with-optimize-option="$(Clang_Optimize_Option)" \
                  --with-extra-ld-options="$(Clang_Linker_Options)" \
		  --without-llvmgcc --without-llvmgxx \
		  --disable-bindings \
		  --disable-doxygen \
		  --disable-zlib \
		  --disable-libedit \
		  --enable-backtraces=no \
		  --enable-libcpp \
		  --enable-clang-plugin-support=no \
		  --with-bug-report-url="http://developer.apple.com/bugreporter/"
Stage1_Configure_Flags = $(Common_Configure_Flags) \
                  --with-extra-options="$(Extra_Options)"
Configure_Flags = $(Common_Configure_Flags) \
                  --with-internal-prefix="$(Install_Prefix)/local" \
                  --with-extra-options="$(Final_Extra_Options)"
Instrumented_Configure_Flags = $(Common_Configure_Flags) \
    --with-extra-options="$(Extra_Options) -fprofile-instr-generate" \
    --with-extra-ld-options="$(Clang_Linker_Options) -fprofile-instr-generate"

CC := $(shell xcrun -find clang)
CXX := $(shell xcrun -find clang++)

# Set the compiler to be used for building stage1.
System_CC := $(CC)
System_CXX := $(CXX)

# Set stage1 GCC_EXEC_PATH path and proper dsymutil tool
Exec_Path := $(shell dirname `xcrun -find ld`)
DSYMUTIL := $(shell xcrun -find dsymutil)

# Set up any additional Clang install targets.
Extra_Clang_Install_Targets := install-minimal-llvm-c
ifeq ($(DT_VARIANT),)
Extra_Clang_Install_Targets += install-clang-diagnostic
endif

# Install /usr/... symlinks?
ifeq ($(Post_Install_RootLinks),1)
Extra_Clang_Install_Targets += install-clang-rootlinks
else ifneq ($(Post_Install_RootLinks),0)
$(error "unknown value for post install of root symlinks: '$(Post_Install_RootLinks)'")
endif

# Install open source license?
ifeq ($(Post_Install_OpenSourceLicense),1)
Extra_Clang_Install_Targets += install-clang-opensourcelicense
else ifneq ($(Post_Install_OpenSourceLicense),0)
$(error "unknown value for post install of open source license: '$(Post_Install_OpenSourceLicense)'")
endif

# Install the magic file to enable internal use of -fobjc-gc?
ifeq ($(Post_Install_EnableObjCGC),1)
Extra_Clang_Install_Targets += install-clang-enableobjcgc
else ifneq ($(Post_Install_EnableObjCGC),0)
$(error "unknown value for post install of enable_objc_gc: '$(Post_Install_EnableObjCGC)'")
endif

# Select stage1 compiler to build.
ifeq ($(Clang_Enable_Bootstrap), 1)
# Always use an x86_64 stage1 compiler -- it is the most well tested and so most
# likely to not be miscompiled by the host compiler.
Stage1_Compiler_Arch := x86_64
endif

# Select final configure target for clang builds.
ifeq ($(Clang_Enable_Bootstrap), 1)
Final_Configure_Target := configure-clang_stage2
else ifeq ($(Clang_Enable_Bootstrap), 0)
Final_Configure_Target := configure-clang_singlestage
else
$(error "invalid setting for clang enable bootstrap: '$(Clang_Enable_Bootstrap)'")
endif

# Set install and build targets.
Install_Target = $(Clang_Make_Variables) $(LLVM_Install_Target)
Build_Target = $(Clang_Make_Variables) $(Clang_Build_Target)

##
# Additional Tool Paths

CHOWN		:= /usr/sbin/chown
FIND		:= /usr/bin/find
INSTALL		:= /usr/bin/install
INSTALL_FILE	:= $(INSTALL) -m 0444
MKDIR		:= /bin/mkdir -p -m 0755
PAX		:= /bin/pax
RMDIR		:= /bin/rm -fr
XARGS		:= /usr/bin/xargs

# We aren't really a recursive make, rather we are a separate build which just
# happens to use make for a sub-task. For that reason, we redefine MAKE to not
# propagate overrides.
MAKE            := env MAKEFLAGS= $(MAKE_COMMAND)

##
# Assorted variables

Sources		= $(SRCROOT)/src
Configure	= $(Sources)/configure
Install_Flags	= DESTDIR=$(OBJROOT)/install-$$arch ONLY_MAN_DOCS=1
Library_SearchPaths = "$(OBJROOT)/stage1-install-$(Stage1_Compiler_Arch)/lib"
ifneq ($(Clang_libLTO_SearchPath),)
	Library_SearchPaths += ":$(Clang_libLTO_SearchPath)"
endif

Stage1_InstallDir = $(OBJROOT)/stage1-install-$(Stage1_Compiler_Arch)
Stage1_CC = $(Stage1_InstallDir)/bin/clang
Stage1_CXX = $(Stage1_InstallDir)/bin/clang++
Stage1_Profdata = $(Stage1_InstallDir)/bin/llvm-profdata

OSV		= $(DSTROOT)/$(Install_Prefix)/local/OpenSourceVersions
OSL		= $(DSTROOT)/$(Install_Prefix)/local/OpenSourceLicenses

##
# Cross-builds need wrapper scripts on the path, so have a local directory
# available for them.

PATH := ${OBJROOT}/bin:${PATH}

# LLVM's makefiles key on RC_ProjectName to enable B&I behavior that we don't
# want for clang builds because clang-ib handles this purpose.
unexport RC_ProjectName

##
# Build Logic

.PHONY: all help install installsrc installhdrs clean

SYSCTL := $(shell if [ `sysctl -n hw.activecpu` -ge 8 -a `sysctl -n hw.memsize` -le 2147483648 ]; then echo 4; else sysctl -n hw.activecpu; fi)

# At this point, we know that Clang_Enable_LTO/Clang_Build_All are either 0 or
# 1, so we don't need to check for improper values.
SYSCTL_FINAL := $(SYSCTL)
ifeq ($(Clang_Enable_LTO),1)
ifeq ($(Clang_Build_All),1)
SYSCTL_FINAL := 2
endif
endif

# Default is to build Clang.
install: install-clang

# We install the source using rsync. We take particular care to:
#
#   1. Exclude any source control files.
#
#   2. Exclude any editor or OS cruft.
#
#   3. Exclude any build products that might be expected to be in a users local
#      tree (e.g., docs/_build) but not in the build directory. We don't try and
#      support users silly enough to do in-tree builds, though.
#
#   4. Exclude all tests.
#
#      This is unfortunate -- we would ideally like to have the exact versions
#      of the tests present at a submission archived -- but XBS does not deal
#      well with projects with many small files and the copy to the submission
#      server takes a very long time without this.
#
#   5. Copy "unsafe" links (links to outside the input directory). This allows
#      developers to link to a properly set up LLVM source directory from
#      Clang-IB and still be able to run buildit or submitproject directly.
installsrc:
	@echo "Installing source..."
	$(_v) $(MKDIR) "$(SRCROOT)"
	$(_v) time rsync -ar . "$(SRCROOT)" \
	  --exclude .git --exclude .svn \
	  --exclude .DS_Store --exclude '*~' --exclude '.*~' \
	  --exclude src/docs/_build \
	  --exclude src/test/ --exclude src/tools/clang/test/ \
	    --exclude 'src/projects/*/test/' \
	  --copy-unsafe-links

# The clean target is run after installing sources, but we do nothing because
# the expectation is that we will just avoid copying in cruft during the installsrc
# phase.
clean:

# We do not need to do anything for the install headers phase.
installhdrs:

##
# Standard Clang Build Support

.PHONY: install-clang
.PHONY: install-clang_final build-clang build-clang_final build-clang_stage1
.PHONY: build-clang_instrumented build-clang_final_ordered
.PHONY: configure-clang_final configure-clang_singlestage configure-clang_stage2
.PHONY: configure-clang_stage1 configure-clang_instrumented
.PHONY: install-clang-rootlinks install-clang-opensourcelicense
.PHONY: install-clang-enableobjcgc
.PHONY: generate-clang-profdata

install-clang: install-clang_final $(Extra_Clang_Install_Targets)

install-clang_final: build-clang
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Installing for $$arch..." && \
		$(MKDIR) $(OBJROOT)/install-$$arch && \
		umask 0022 && \
		$(MAKE) -C $(OBJROOT)/$$arch \
		  $(Install_Flags) $(Install_Target) || exit 1 ; \
	done
	$(_v) rm -rf $(DSTROOT)
	$(_v) mkdir -p $(DSTROOT)
	$(_v) for arch in $(filter-out $(firstword $(RC_ARCHS)),$(RC_ARCHS)); do \
		rm -rf $(OBJROOT)/install-$$arch$(Install_Prefix)/lib/clang/*/lib; \
	done
	./merge-lipo `for arch in $(RC_ARCHS) ; do echo $(OBJROOT)/install-$$arch ; done` $(DSTROOT)
	$(_v) ln -sf clang $(DSTROOT)/$(Install_Prefix)/bin/cc
	$(_v) ln -sf clang.1 $(DSTROOT)/$(Install_Prefix)/share/man/man1/cc.1
	$(_v) ln -sf clang++ $(DSTROOT)/$(Install_Prefix)/bin/c++
	$(_v) ln -sf clang++.1 $(DSTROOT)/$(Install_Prefix)/share/man/man1/c++.1
	$(_v) ln -sf clang.1 $(DSTROOT)/$(Install_Prefix)/share/man/man1/clang++.1
	$(_v) ln -sf llvm-dsymutil $(DSTROOT)/$(Install_Prefix)/bin/dsymutil
	$(_v) ln -sf llvm-cov $(DSTROOT)/$(Install_Prefix)/bin/gcov
	# REMINDER: The llvm-cov.1 file here needs to be regenerated manually
	# whenever the llvm-cov documentation changes!!
	$(INSTALL_FILE) $(SRCROOT)/llvm-cov.1 $(DSTROOT)/$(Install_Prefix)/share/man/man1/llvm-cov.1
	$(_v) ln -sf llvm-cov.1 $(DSTROOT)/$(Install_Prefix)/share/man/man1/gcov.1
	$(_v) $(FIND) $(DSTROOT) -perm -0111 -name '*.a' | $(XARGS) chmod a-x
	@echo "Copying executables into SYMROOT..."
	$(_v) cd $(DSTROOT) && find . -perm -0111 -type f -print | cpio -pdm $(SYMROOT)
	@echo "Running 'dsymutil' on executables (in SYMROOT)..."
	$(_v) cd $(SYMROOT) && find . -perm -0111 -type f -print | \
	  xargs -n 1 -P $(SYSCTL) $(DSYMUTIL)
	@echo "Stripping executables in DSTROOT..."
	$(_v) find $(DSTROOT) -perm -0111 -type f -print | xargs -n 1 -P $(SYSCTL) strip -S
	@echo "Setting permissions for executables in DSTROOT..."
	$(_v)- $(CHOWN) -R root:wheel $(DSTROOT) $(SYMROOT)

build-clang: $(Final_Build_Target)

build-clang_final: configure-clang_final
	$(_v) set -ex; \
	for arch in $(RC_ARCHS) ; do \
	  echo "Building (Final) for $$arch..."; \
	  if [ -f $(SRCROOT)/buildbot-order-files/$$arch/clang.order ]; then \
	    order_file=$(SRCROOT)/buildbot-order-files/$$arch/clang.order; \
	  else  \
	    order_file=$(SRCROOT)/static-order-files/$$arch/clang.order; \
	  fi; \
	  time $(MAKE) -j$(SYSCTL_FINAL) -C $(OBJROOT)/$$arch \
	    $(Build_Target) CLANG_ORDER_FILE=$${order_file} \
	    DYLD_LIBRARY_PATH="$(Library_SearchPaths)" ; \
	done

# This is a special target which uses the build compiler to generate order file
# information, and then rebuilds the compiler with the constructed order file.
build-clang_final_ordered: build-clang_final
	set -ex && \
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building (Final) (Ordered) for $$arch..." && \
	        echo "Generating Order File Data for $$arch " && \
	        $(SRCROOT)/order-files/gen-clang-order-data \
		  --no-sudo \
	          --cc "$(OBJROOT)/$$arch/$(Build_Mode)/bin/clang" \
		  --inputs "$(SRCROOT)/order-files/inputs" \
		  --temps "$(OBJROOT)/order-data/$$arch/temps" \
		  --outputs "$(OBJROOT)/order-data/$$arch/data"; \
	        echo "Generating Order File for $$arch" && \
	        $(SRCROOT)/order-files/gen-order-file \
	          --binary "$(OBJROOT)/$$arch/$(Build_Mode)/bin/clang" \
	          --output "$(OBJROOT)/order-data/$$arch/clang.order" \
	          --output-unordered-symbols \
		    "$(OBJROOT)/order-data/$$arch/unordered_symbols.txt" \
	          --method "call_order" \
		  "$(OBJROOT)/order-data/$$arch/"data*.log && \
	        echo "Rebuilding With Order File" && \
		mv "$(OBJROOT)/$$arch/$(Build_Mode)/bin/clang" \
		  "$(OBJROOT)/$$arch/$(Build_Mode)/bin/clang.preorder" && \
		$(MAKE) -j$(SYSCTL_FINAL) \
		  -C "$(OBJROOT)/$$arch/tools/clang/tools/driver" \
		  $(Clang_Make_Variables) \
		  "CLANG_ORDER_FILE=$(OBJROOT)/order-data/$$arch/clang.order" \
	          DYLD_LIBRARY_PATH="$(Library_SearchPaths)" ; \
	done

$(OBJROOT)/clang.profdata: $(SRCROOT)/clang.profdata.bz2
	bzcat "$(SRCROOT)/clang.profdata.bz2" > $@

generate-clang-profdata: build-clang_instrumented $(Stage1_Profdata)
	$(_v) set -ex && \
	echo "Generating Profile Data" && \
        $(SRCROOT)/order-files/gen-clang-order-data \
	  --profdata --no-sudo \
	  --cc "$(OBJROOT)/instrumented-install-$(Stage1_Compiler_Arch)/bin/clang" \
	  --inputs "$(SRCROOT)/order-files/inputs" \
	  --temps "$(OBJROOT)/profdata/temps" \
	  --outputs "$(OBJROOT)/profdata/data"
	$(Stage1_Profdata) merge -o "$(OBJROOT)/tmp.profdata" \
	  "$(OBJROOT)"/profdata/*.profraw
	bzip2 < "$(OBJROOT)/tmp.profdata" > "$(SRCROOT)/clang.profdata.bz2"
	rm -f "$(OBJROOT)/tmp.profdata"

build-clang_stage1: configure-clang_stage1
	$(_v) set -ex; \
	$(_v) for arch in $(Stage1_Compiler_Arch) ; do \
	  $(_v) echo "Building (Stage 1) for $$arch..."; \
	  if [ -f $(SRCROOT)/buildbot-order-files/$$arch/clang.order ]; then \
	    order_file=$(SRCROOT)/buildbot-order-files/$$arch/clang.order; \
	  else  \
	    order_file=$(SRCROOT)/static-order-files/$$arch/clang.order; \
	  fi; \
	  $(_v) time $(MAKE) -j$(SYSCTL) -C $(OBJROOT)/stage1-$(Stage1_Compiler_Arch) \
	    $(Clang_Make_Variables) $(Clang_Only_Build_Target) \
	    CLANG_ORDER_FILE=$${order_file}; \
	  $(_v) time $(MAKE) -j$(SYSCTL) -C $(OBJROOT)/stage1-$(Stage1_Compiler_Arch) \
	    $(Install_Target); \
	done

build-clang_instrumented: configure-clang_instrumented
	$(_v) set -ex; \
	$(_v) echo "Building (Instrumented) for $$arch..."; \
	$(_v) time $(MAKE) -j$(SYSCTL) -C $(OBJROOT)/instrumented-$(Stage1_Compiler_Arch) \
	  $(Clang_Make_Variables) $(Clang_Only_Build_Target); \
	$(_v) time $(MAKE) -j$(SYSCTL) -C $(OBJROOT)/instrumented-$(Stage1_Compiler_Arch) \
	  $(Install_Target)

configure-clang_final: $(Final_Configure_Target)

configure-clang_stage2: $(Stage1_CC)
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Final) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		time $(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
		  CC="$(Stage1_CC) -B$(Exec_Path) -arch $$arch" \
		  CXX="$(Stage1_CXX) -B$(Exec_Path) -arch $$arch" || exit 1 ; \
	done

configure-clang_singlestage:
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Final) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		time $(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
		  CC="$(System_CC) -arch $$arch" \
		  CXX="$(System_CXX) -arch $$arch" || exit 1 ; \
	done

configure-clang_stage1: 
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) echo "Configuring (Stage 1) for $(Stage1_Compiler_Arch)..."
	$(_v) $(MKDIR) $(OBJROOT)/stage1-$(Stage1_Compiler_Arch)
	$(_v) cd $(OBJROOT)/stage1-$(Stage1_Compiler_Arch) && \
	      time $(Configure) --prefix="$(OBJROOT)/stage1-install-$(Stage1_Compiler_Arch)" $(Stage1_Configure_Flags) \
	        CC="$(System_CC) -arch $(Stage1_Compiler_Arch)" \
		CXX="$(System_CXX) -arch $(Stage1_Compiler_Arch)" || exit 1

$(Stage1_CC): build-clang_stage1
$(Stage1_CXX): build-clang_stage1
$(Stage1_Profdata): build-clang_stage1

configure-clang_instrumented: $(Stage1_CC) $(Stage1_CXX)
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) echo "Configuring (Instrumented) for $(Stage1_Compiler_Arch)..."
	$(_v) $(MKDIR) $(OBJROOT)/instrumented-$(Stage1_Compiler_Arch) && \
	$(_v) cd $(OBJROOT)/instrumented-$(Stage1_Compiler_Arch) && \
	      time $(Configure) --prefix="$(OBJROOT)/instrumented-install-$(Stage1_Compiler_Arch)" \
	        $(Instrumented_Configure_Flags) \
		CC="$(Stage1_CC) -B$(Exec_Path) -arch $(Stage1_Compiler_Arch)" \
		CXX="$(Stage1_CXX) -B$(Exec_Path) -arch $(Stage1_Compiler_Arch)" || exit 1

install-clang-rootlinks: install-clang_final
	$(MKDIR) -p $(DSTROOT)/usr/bin
	$(MKDIR) -p $(DSTROOT)/usr/share/man/man1
	ln -sf ../../$(Install_Prefix)/bin/clang $(DSTROOT)/usr/bin/clang
	cp $(DSTROOT)/$(Install_Prefix)/share/man/man1/clang.1 $(DSTROOT)/usr/share/man/man1/
	if [ -f $(DSTROOT)/$(Install_Prefix)/bin/clang++ ]; then \
	  ln -sf ../../$(Install_Prefix)/bin/clang++ $(DSTROOT)/usr/bin/clang++; \
	  ln -sf clang.1 $(DSTROOT)/usr/share/man/man1/clang++.1; \
	fi
	if [ -f $(DSTROOT)/$(Install_Prefix)/lib/libLTO.dylib ]; then \
	  $(MKDIR) -p $(DSTROOT)/usr/lib; \
	  ln -sf ../../$(Install_Prefix)/lib/libLTO.dylib $(DSTROOT)/usr/lib/libLTO.dylib; \
	fi

install-clang-opensourcelicense: install-clang_final
	$(MKDIR) $(OSV)
	$(INSTALL_FILE) $(SRCROOT)/clang.plist $(OSV)/clang.plist
	$(MKDIR) $(OSL)
	$(INSTALL_FILE) $(Sources)/LICENSE.TXT $(OSL)/clang-llvm.txt
	$(INSTALL_FILE) $(Sources)/tools/clang/LICENSE.TXT $(OSL)/clang.txt

install-clang-enableobjcgc: install-clang_final
	$(MKDIR) $(DSTROOT)/$(Install_Prefix)/local/lib/clang
	echo 1 > $(DSTROOT)/$(Install_Prefix)/local/lib/clang/enable_objc_gc

install-clang-links:
	$(MKDIR) -p $(DSTROOT)/$(Install_Prefix)/bin
	ln -sf ../../../../../usr/bin/clang $(DSTROOT)/$(Install_Prefix)/bin/clang
	ln -sf ../../../../../usr/bin/clang++ $(DSTROOT)/$(Install_Prefix)/bin/clang++

install-minimal-llvm-c:
	$(MKDIR) -p $(DSTROOT)/$(Install_Prefix)/local/include/llvm-c
	$(INSTALL_FILE) $(Sources)/include/llvm-c/ProfileData.h $(DSTROOT)/$(Install_Prefix)/local/include/llvm-c
	$(INSTALL_FILE) $(Sources)/include/llvm-c/Support.h $(DSTROOT)/$(Install_Prefix)/local/include/llvm-c
	$(INSTALL_FILE) $(Sources)/include/llvm-c/lto.h $(DSTROOT)/$(Install_Prefix)/local/include/llvm-c

# We install a copy of the clang-parse-diagnostics-file utility into
# /usr/local/bin, for use by B&I's XBS_SEND_WARNINGS_EMAIL parameter.
#
# This file always installs to /usr/local/bin.
install-clang-diagnostic:
	$(MKDIR) -p $(DSTROOT)/usr/local/bin
	$(INSTALL) $(Sources)/utils/clang-parse-diagnostics-file $(DSTROOT)/usr/local/bin/

##
# Cross Compilation Build Support

.PHONY: install-cross build-cross configure-cross setup-tools-cross

install-cross: build-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Installing for $$arch..." && \
		$(MKDIR) $(OBJROOT)/install-$$arch && \
		umask 0022 && \
		$(MAKE) -C $(OBJROOT)/$$arch \
		  $(Install_Flags) $(Install_Target) || exit 1 ; \
	done
	$(_v) rm -rf $(DSTROOT)
	$(_v) mkdir -p $(DSTROOT)
	$(_v) for arch in $(filter-out $(firstword $(RC_ARCHS)),$(RC_ARCHS)); do \
		rm -rf $(OBJROOT)/install-$$arch$(Install_Prefix)/lib/clang/*/lib; \
		rm -f $(OBJROOT)/install-$$arch$(Install_Prefix)/bin/llvm-config-host; \
	done
	./merge-lipo `for arch in $(RC_ARCHS) ; do echo $(OBJROOT)/install-$$arch ; done` $(DSTROOT)
	$(_v) $(FIND) $(DSTROOT) -perm -0111 -name '*.a' | $(XARGS) chmod a-x
	@echo "Copying executables into SYMROOT..."
	$(_v) cd $(DSTROOT) && find . -perm -0111 -type f -print | cpio -pdm $(SYMROOT)
	@echo "Running 'dsymutil' on executables (in SYMROOT)..."
	$(_v) cd $(SYMROOT) && find . -perm -0111 -type f -print | \
	  xargs -n 1 -P $(SYSCTL) $(DSYMUTIL)
	@echo "Stripping executables in DSTROOT..."
	$(_v) find $(DSTROOT) -perm -0111 -type f -print | xargs -n 1 -P $(SYSCTL) strip -S
	@echo "Setting permissions for executables in DSTROOT..."
	$(_v)- $(CHOWN) -R root:wheel $(DSTROOT) $(SYMROOT)

build-cross: configure-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building (Cross) for $$arch..." && \
		$(MAKE) -j$(SYSCTL) -C $(OBJROOT)/$$arch $(Build_Target) CFLAGS="-arch $$arch $(CFLAGS)" CXXFLAGS="-arch $$arch $(CXXFLAGS)" SDKROOT= || exit 1; \
	done

configure-cross: setup-tools-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Cross) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		unset SDKROOT && \
		$(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
			--host=arm-apple-darwin10 \
			--target=arm-apple-darwin10 \
			--build=i686-apple-darwin10 \
			--program-prefix="" \
			--enable-libcpp \
		  || exit 1 ; \
	done

# A cross-compiler configure will expect to find tools under names like
# arm-apple-darwin10-gcc, so make sure we have them.  Also add sym links
# for the host tools so we are sure to use the right versions of them.
setup-tools-cross:
	$(_v) $(MKDIR) $(OBJROOT)/bin
	$(_v) for prog in ar nm ranlib strip lipo ld as ; do \
	  ln -s `xcrun -find $$prog` $(OBJROOT)/bin/$$prog && \
	  echo '#!/bin/sh' > $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo 'exec '`xcrun -sdk $(SDKROOT) -find $$prog` '"$$@"' \
	    >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  chmod a+x $(OBJROOT)/bin/arm-apple-darwin10-$$prog || exit 1; \
	done
	$(_v) for gcc in gcc g++ ; do \
	  cc=`echo $$gcc | sed -e 's/gcc/cc/' -e 's/g/c/'` && \
	  clang=`echo $$gcc | sed -e 's/gcc/clang/' -e 's/^g/clang/'` && \
	  prog=`xcrun -find $$cc` && \
	  sysroot=`xcrun -sdk $(SDKROOT) --show-sdk-path` && \
	  ln -s $$prog $(OBJROOT)/bin/$$gcc && \
	  ln -s $$prog $(OBJROOT)/bin/$$clang && \
	  script=$(OBJROOT)/bin/arm-apple-darwin10-$$gcc && \
	  echo '#!/bin/sh' > $$script && \
	  echo "ARCH='-arch armv7'" >> $$script && \
	  echo 'for i in $$@ ; do if [ "$$i" == "-arch" ] ; then' \
	    ' ARCH= ; fi ; done' >> $$script && \
	  echo exec $$prog '$$ARCH -isysroot '$$sysroot' "$$@"' >> $$script &&\
	  chmod a+x $$script || exit 1 ; \
	done

###
# Utility targets

commit-buildbot-order-file:
	@if ([ ! -d .svn ] || [ ! -f ClangBNI.mk ] || \
	     [ ! -d buildbot-order-files ]); then \
	  echo "error: this does not look like a valid Apple Clang checkout"; \
	  exit 1; \
	fi
	@if ([ -z "$(OBJROOT)" ] || [ ! -d "$(OBJROOT)" ]); then \
	  echo "error: OBJROOT must point to a valid directory"; \
	  exit 1; \
	fi
	@echo "Updating current order files to HEAD (to avoid conflicts)."
	@(cd buildbot-order-files && svn up)
	@echo "Copying order files from OBJROOT to the current checkout."
	@for path in "$(OBJROOT)"/order-data/*; do \
	  arch=$$(basename $$path); \
	  if [ ! -f $$path/clang.order ]; then \
	    echo "warning: missing order file for arch $$arch"; \
	  fi; \
	  mkdir -p buildbot-order-files/$$arch; \
	  cp $$path/clang.order buildbot-order-files/$$arch/; \
	done
	@echo "Committing result."
	(set -ex; \
	 cd buildbot-order-files && \
	 svn add -N */ && \
	 svn add */clang.order && \
	 if (! svn commit -m "[autoupdate] Update buildbot order files."); then \
	   echo "warning: svn commit failed, restoring contents!"; \
	   svn revert */ || true; \
	   rm -rf */; \
	   svn up; \
	 fi)
.PHONY: commit-buildbot-order-file

###
# Debugging

# General debugging rule, use 'make dbg-print-XXX' to print the
# definition, value and origin of XXX.
make-print-%:
	$(error PRINT: $(value $*) = "$($*)" (from $(origin $*)))
