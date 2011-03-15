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
#   Clang_Driver_Mode := {Production, Development}
#     Enable/disable the "production" driver mode.
#
#   Clang_Enable_CXX := {0, 1}
#     When in production driver mode, enable C++ support.
#
#   Clang_Extra_Options := ...
#     Additional options to pass to make.
#
#   Clang_Optimize_Option := ...
#     The optimization flags to use.

# This makefile currently supports the following build targets:
#
#   install-clang	 - Build the Clang compiler.
#   install-cross	 - Build the Clang compiler, for ARM.
#   install-libclang	 - Build the libclang dylib.
#   install-clang-links	 - Install links from a platforms subdirectory to the
#                          root clang.
#
# The default build target is 'install-clang'.
#
# Particular build targets (which correspond to project configurations) may
# override various defaults based on what makes sense to build/install for that
# particular target.

##
# Variable defaults.

# Install to /Developer by default.
Default_Install_Root := /Developer
# Don't install root links or license.
Post_Install_RootLinks := 0
Post_Install_OpenSourceLicense := 0
# Install to .../usr
Install_Path_Suffix := usr
# Include x86 and ARM backends.
LLVM_Backends := x86,arm
# Don't use extra make variables.
Extra_Make_Variables :=
# Don't install any archive files.
Extra_Make_Variables += NO_INSTALL_ARCHIVES=1
# LLVM level install target is 'install-clang.
LLVM_Install_Target := install-clang

##
# Per Project/Target Configuration

ifeq ($(MAKECMDGOALS),install-cross)

# Install to / by default.
Default_Install_Root := /
# Don't build compiler-rt.
Extra_Make_Variables += CLANG_NO_RUNTIME=1
# Never bootstrap.
Clang_Enable_Bootstrap := 0

else ifeq ($(MAKECMDGOALS),install-libclang)

# Install to 'clang-ide' subdirectory.
Install_Path_Suffix := usr/clang-ide
# Only include x86 backend.
LLVM_Backends := x86
# Don't build compiler-rt.
Extra_Make_Variables += CLANG_NO_RUNTIME=1
# Use install-clang-c install target.
LLVM_Install_Target := install-clang-c
# Never bootstrap.
Clang_Enable_Bootstrap := 0

else ifeq ($(MAKECMDGOALS),install-clang-links)

# Dummy project which only installs compiler links from the INSTALL_LOCATION to
# the primary SDK compiler.

else

# Default project (clang).

# Install root links and license when no install location is set.
ifeq ($(INSTALL_LOCATION),)
Post_Install_RootLinks := 1
Post_Install_OpenSourceLicense := 1
endif

endif

##
# B&I Build Logic
##

# Require Train_Name to be set.
ifeq ($(Train_Name),)
$(error "invalid setting for train name: '$(Train_Name)'")
endif

# Require Source_To_Draw_From to be set to a known value.
ifeq ($(Source_To_Draw_From),trunk)
Draw_LLVM_From_Trunk := 1
Draw_Clang_From_Trunk := 1
else ifeq ($(Source_To_Draw_From),branch)
Draw_LLVM_From_Trunk := 0
Draw_Clang_From_Trunk := 0
else ifeq ($(Source_To_Draw_From),branch-llvm-only)
Draw_LLVM_From_Trunk := 0
Draw_Clang_From_Trunk := 1
else
$(error "invalid setting for source to draw from: '$(Source_To_Draw_From)'")
endif

# Require Clang_Version to be set.
ifeq ($(Clang_Version),)
$(error "invalid setting for clang version: '$(Clang_Version)'")
endif

# Set RC_ProjectSourceVersion, if unspecified.
ifeq ($(RC_ProjectSourceVersion),)
RC_ProjectSourceVersion := 99999.99
$(warning "setting dummy RC_ProjectSourceVersion: '$(RC_ProjectSourceVersion)'")
endif

# Select assertions mode.
ifeq ($(Clang_Use_Assertions), 1)
Assertions_Configure_Flag :=  --enable-assertions
else ifeq ($(Clang_Use_Assertions), 0)
Assertions_Configure_Flag :=  --disable-assertions
else
$(error "invalid setting for clang assertions: '$(Clang_Use_Assertions)'")
endif

# Select whether to build everything (for testing purposes).
ifeq ($(Clang_Build_All), 1)
Clang_Build_Target := all
else ifeq ($(Clang_Build_All), 0)
Clang_Build_Target := clang-only
else
$(error "invalid setting for clang build all mode: '$(Clang_Build_All)'")
endif

# Set makefile variables to pass during build and install.
Clang_Make_Variables := $(Extra_Make_Variables) KEEP_SYMBOLS=1 \
                        CLANG_VENDOR=Apple \
                        CLANG_VENDOR_UTI=com.apple.compilers.llvm.clang
Clang_Make_Variables += CLANG_VERSION=$(Clang_Version)
Clang_Make_Variables += CLANG_ORDER_FILE=$(SRCROOT)/clang.order
ifeq ($(Clang_Driver_Mode), Production)
Clang_Make_Variables += CLANG_IS_PRODUCTION=1

ifeq ($(Clang_Enable_CXX), 1)
Clang_Make_Variables += CLANGXX_IS_PRODUCTION=1
else ifeq ($(Clang_Enable_CXX), 0)
# ... this is the default ...
else
$(error "invalid setting for clang enable C++: '$(Clang_Enable_CXX)'")
endif

# Set LLVM_VERSION_INFO make variable. We do this here because setting it in the
# CC options for configure ends up breaking tests that can't be bothered to
# quote things properly, and that is too hard to fix.
Clang_Make_Variables += \
  LLVM_VERSION_INFO="from Apple Clang $(Clang_Version) (build $(RC_ProjectSourceVersion))"

else ifeq ($(Clang_Driver_Mode), Development)
# ... this is the default ...
else
$(error "invalid setting for clang driver mode: '$(Clang_Driver_Mode)'")
endif

# Set destination information.
ifneq ($(INSTALL_LOCATION),)
Install_Root := $(INSTALL_LOCATION)
else ifneq ($(Default_Install_Root),)
Install_Root := $(Default_Install_Root)
else
$(error "invalid setting for default install root: '$(Default_Install_Root)'")
endif

# Set Install_Prefix and validate it exists.
Install_Prefix := $(Install_Root)
ifneq (OK, $(shell test -d $(Install_Prefix) && echo "OK"))
  $(error "invalid install prefix: '$(Install_Prefix)'")
endif

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

# Set configure flags.
Common_Configure_Flags = \
		  --enable-targets=$(LLVM_Backends) \
		  --enable-optimized \
		  --disable-timestamps \
		  $(Assertions_Configure_Flag) \
                  --with-optimize-option="$(Clang_Optimize_Option)" \
		  --without-llvmgcc --without-llvmgxx \
		  --disable-bindings \
		  --disable-doxygen
Stage1_Configure_Flags = $(Common_Configure_Flags) \
                  --with-extra-options="$(Extra_Options)"
Configure_Flags = $(Common_Configure_Flags) \
                  --with-extra-options="$(Extra_Options) $(Clang_Final_Extra_Options)"

# Determine the /Developer/usr/bin/clang major build version number
SysClangMajorBuildVersion := \
  $(shell /Developer/usr/bin/clang -v 2>&1 | \
	head -1 | \
	sed -e "s@.*\(clang-[0-9]*\).*@\1@" \
	    -e "s@\$$@-@" | \
	cut -d- -f2 | \
	cut -d. -f1)
ifneq (x$(SysClangMajorBuildVersion),x)
ifeq ($(shell test $(SysClangMajorBuildVersion) -ge 115 && echo OK),OK)
CC := /Developer/usr/bin/clang
CXX := /Developer/usr/bin/clang++
endif
endif

# Set stage1 compiler.
Stage1_CC := $(CC)
Stage1_CXX := $(CXX)

# Set up any additional Clang install targets.
Extra_Clang_Install_Targets := install-lto-h

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
Install_Target_Stage1 = $(Clang_Make_Variables) install-clang
Build_Target_Stage1 = $(Clang_Make_Variables) clang-only

# Set default target.

all: install

###
# Utility targets for managing the integration branch.

# Determine if we are running an SVN utility target.
SVN_UTILITY_TARGETS := \
	test-svn update-sources update-sources-from-tag \
	rebranch-llvm-from-tag rebranch-clang-from-tag \
	rebranch-clang-from-revision \
	tag-clang retag-clang
ifneq ($(strip $(foreach i,$(SVN_UTILITY_TARGETS), $(filter $(i),$(MAKECMDGOALS)))),)
SVN_UTILITY_MODE := 1
$(warning NOTE: Running SVN utility target. Be careful!)
$(warning )
endif

ifeq ($(SVN_UTILITY_MODE),1)
SVN_BASE := $(shell svn info | sed -n 's/^URL: //; s,/llvm-project/.*$$,/llvm-project,p')
SVN_CLANG := $(shell svn info | sed -n 's/^URL: //p')
SVN_TAGS := $(SVN_BASE)/cfe/tags/Apple

$(warning Using SVN base     : $(SVN_BASE))
$(warning Using Clang SVN    : $(SVN_CLANG))
$(warning Using SVN tag dir  : $(SVN_TAGS))
$(warning )

# Validate that we match the expected branch name, as a safety/sanity check.
ifneq ($(SVN_CLANG),$(SVN_BASE)/cfe/branches/Apple/$(Train_Name)-IB)
$(error Unable to recognize SVN layout, conservatively refusing to do anything.)
endif

# Define the upstream paths.
LLVM_Branch_Path := $(SVN_BASE)/llvm/branches/Apple/$(Train_Name)
Clang_Branch_Path := $(SVN_BASE)/cfe/branches/Apple/$(Train_Name)

ifeq ($(Draw_LLVM_From_Trunk),1)
LLVM_Upstream := $(SVN_BASE)/llvm/trunk
else
LLVM_Upstream := $(LLVM_Branch_Path)
endif

ifeq ($(Draw_Clang_From_Trunk),1)
Clang_Upstream := $(SVN_BASE)/cfe/trunk
else
Clang_Upstream := $(Clang_Branch_Path)
endif

CompilerRT_Upstream := $(SVN_BASE)/compiler-rt/trunk

# Print information on the upstream sources.
$(warning LLVM Upstream      : $(LLVM_Upstream))
$(warning Clang Upstream     : $(Clang_Upstream))
$(warning CompilerRT Upstream: $(CompilerRT_Upstream))
$(warning )

# Only actually do anything when EXECUTE=1
ifeq ($(EXECUTE), 1)
SVN_COMMAND := svn
else
$(warning Not in commit mode, only echoing commands (use EXECUTE=1 to execute).)
$(warning )
SVN_COMMAND := @echo svn
endif

else
SVN_COMMAND := @echo "NOT IN SVN COMMAND MODE!!!"
endif

test-svn:
	@echo "*** TESTING SVN UTILITY MODE ***"
	$(SVN_COMMAND) info $(SVN_BASE)

update-sources:
	@if ! [ -n "$(REVISION)" ]; then \
	  echo Usage: make $@ REVISION=102052; \
	  false; \
	fi
	$(SVN_COMMAND) rm -m 'Update.' $(SVN_CLANG)/src
	$(SVN_COMMAND) cp -m 'Update.' $(LLVM_Upstream)@$(REVISION) $(SVN_CLANG)/src
	$(SVN_COMMAND) cp -m 'Update.' $(Clang_Upstream)@$(REVISION) $(SVN_CLANG)/src/tools/clang
	$(SVN_COMMAND) cp -m 'Update.' $(CompilerRT_Upstream)@$(REVISION) $(SVN_CLANG)/src/projects/compiler-rt
	$(SVN_COMMAND) up

update-sources-from-tag:
	@if ! [ -n "$(VERSION)" ]; then \
	  echo Usage: make $@ VERSION=122; \
	  false; \
	fi
	$(SVN_COMMAND) rm -m 'Update.' $(SVN_CLANG)/src
	$(SVN_COMMAND) cp -m 'Update from clang-$(VERSION).' $(SVN_TAGS)/clang-$(VERSION)/src $(SVN_CLANG)/src
	$(SVN_COMMAND) up

rebranch-llvm-from-tag:
	@if ! [ -n "$(VERSION)" ]; then \
	  echo Usage: make $@ VERSION=65; \
	  false; \
	fi
	$(SVN_COMMAND) rm -m 'Remove for branch of LLVM.' $(LLVM_Branch_Path)
	$(SVN_COMMAND) cp -m 'Rebranch LLVM from clang-$(VERSION).' $(SVN_TAGS)/clang-$(VERSION)/src $(LLVM_Branch_Path)
	$(SVN_COMMAND) rm -m 'Rebranch LLVM from clang-$(VERSION) (cleanup 1/2)' $(LLVM_Branch_Path)/tools/clang
	$(SVN_COMMAND) rm -m 'Rebranch LLVM from clang-$(VERSION) (cleanup 2/2)' $(LLVM_Branch_Path)/projects/compiler-rt

rebranch-clang-from-tag:
	@if ! [ -n "$(VERSION)" ]; then \
	  echo Usage: make $@ VERSION=65; \
	  false; \
	fi
	$(SVN_COMMAND) rm -m 'Remove for branch of Clang.' $(Clang_Branch_Path)
	$(SVN_COMMAND) cp -m 'Rebranch Clang from clang-$(VERSION).' $(SVN_TAGS)/clang-$(VERSION)/src/tools/clang $(Clang_Branch_Path)

rebranch-clang-from-revision:
	@if ! [ -n "$(REVISION)" ]; then \
	  echo Usage: make $@ REVISION=100000; \
	  false; \
	fi
	$(SVN_COMMAND) rm -m 'Remove for branch of Clang.' $(Clang_Branch_Path)
	$(SVN_COMMAND) cp -m 'Rebranch Clang from clang trunk at r$(REVISION).' $(SVN_BASE)/cfe/trunk@$(REVISION) $(Clang_Branch_Path)

tag-clang:
	@if ! [ -n "$(VERSION)" ]; then \
	  echo Usage: make $@ VERSION=25; \
	  false; \
	fi
	$(SVN_COMMAND) cp -m 'Tag.' $(SVN_CLANG) $(SVN_TAGS)/clang-$(VERSION)

retag-clang:
	@if ! [ -n "$(VERSION)" ]; then \
	  echo Usage: make $@ VERSION=25; \
	  false; \
	fi
	$(SVN_COMMAND) rm -m 'Retag.' $(SVN_TAGS)/clang-$(VERSION)
	$(SVN_COMMAND) cp -m 'Retag.' $(SVN_CLANG) $(SVN_TAGS)/clang-$(VERSION)

##
# Additional Tool Paths

CHOWN		:= /usr/sbin/chown
CXX             := /usr/bin/g++
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
# Tool Variables

Cruft      = CVS RCS SCCS *~ .*~ .nfs\* .*.wmd .svn .DS_Store
Find_Cruft = '(' $(Cruft:%=-name '%' -or) -name '' ')' -print

##
# Assorted variables

Sources		= $(SRCROOT)/src
Configure	= $(Sources)/configure
Install_Flags	= DESTDIR=$(OBJROOT)/install-$$arch ONLY_MAN_DOCS=1

OSV		= $(DSTROOT)/usr/local/OpenSourceVersions
OSL		= $(DSTROOT)/usr/local/OpenSourceLicenses

##
# Cross-builds need wrapper scripts on the path, so have a local directory
# available for them.

PATH := ${OBJROOT}/bin:${PATH}

##
# Build Logic

.PHONY: all install installsrc installhdrs clean

SYSCTL := $(shell if [ `sysctl -n hw.activecpu` -ge 8 -a `sysctl -n hw.memsize` -le 2147483648 ]; then echo 4; else sysctl -n hw.activecpu; fi)

# Default is to build Clang.
install: install-clang

clean:

installsrc:
	@echo "Installing source..."
	$(_v) $(MKDIR) "$(SRCROOT)"
	$(_v) $(PAX) -rw . "$(SRCROOT)"
	$(_v) $(FIND) "$(SRCROOT)" $(Find_Cruft) -depth -exec $(RMDIR) "{}" \;
	$(_v) rm -rf "$(SRCROOT)"/src/test/*/
	$(_v) rm -rf "$(SRCROOT)"/src/tools/clang/test/*/

installhdrs:

##
# Standard Clang Build Support

.PHONY: install-clang install-libclang
.PHONY: install-clang_final build-clang build-clang_final build-clang_stage1
.PHONY: configure-clang_final configure-clang_singlestage configure-clang_stage2
.PHONY: configure-clang_stage1
.PHONY: install-clang-rootlinks install-clang-opensourcelicense

install-clang: install-clang_final $(Extra_Clang_Install_Targets)

install-libclang: install-clang_final $(Extra_Clang_Install_Targets)

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
	$(_v) $(FIND) $(DSTROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(SYMROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(DSTROOT) -perm -0111 -type f -print | $(XARGS) -n 1 -P $(SYSCTL) dsymutil
	$(_v) cd $(DSTROOT) && find . -path \*.dSYM/\* -print | cpio -pdml $(SYMROOT)
	$(_v) find $(DSTROOT) -perm -0111 -type f -print | xargs -P $(SYSCTL) strip -S
	$(_v) find $(DSTROOT) -name \*.dSYM -print | xargs rm -r
	$(_v)- $(CHOWN) -R root:wheel $(DSTROOT) $(SYMROOT)

build-clang: build-clang_final

build-clang_final: configure-clang_final
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building (Final) for $$arch..." && \
		time $(MAKE) -j$(SYSCTL) -C $(OBJROOT)/$$arch $(Build_Target) || exit 1; \
	done

build-clang_stage1: configure-clang_stage1
	$(_v) echo "Building (Stage 1) for $(Stage1_Compiler_Arch)..."
	$(_v) time $(MAKE) -j$(SYSCTL) -C $(OBJROOT)/stage1-$(Stage1_Compiler_Arch) $(Build_Target_Stage1)
	$(_v) time $(MAKE) -j$(SYSCTL) -C $(OBJROOT)/stage1-$(Stage1_Compiler_Arch) $(Install_Target_Stage1)

configure-clang_final: $(Final_Configure_Target)

configure-clang_stage2: build-clang_stage1
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Final) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		time $(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
		  CC="$(OBJROOT)/stage1-install-$(Stage1_Compiler_Arch)/bin/clang -arch $$arch" \
		  CXX="$(OBJROOT)/stage1-install-$(Stage1_Compiler_Arch)/bin/clang++ -arch $$arch" || exit 1 ; \
	done

configure-clang_singlestage:
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Final) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		time $(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
		  CC="$(Stage1_CC) -arch $$arch" \
		  CXX="$(Stage1_CXX) -arch $$arch" || exit 1 ; \
	done

configure-clang_stage1: 
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) echo "Configuring (Stage 1) for $(Stage1_Compiler_Arch)..."
	$(_v) $(MKDIR) $(OBJROOT)/stage1-$(Stage1_Compiler_Arch)
	$(_v) cd $(OBJROOT)/stage1-$(Stage1_Compiler_Arch) && \
	      time $(Configure) --prefix="$(OBJROOT)/stage1-install-$(Stage1_Compiler_Arch)" $(Stage1_Configure_Flags) \
	        CC="$(Stage1_CC) -arch $(Stage1_Compiler_Arch)" \
		CXX="$(Stage1_CXX) -arch $(Stage1_Compiler_Arch)" || exit 1

install-clang-rootlinks: install-clang_final
	$(MKDIR) -p $(DSTROOT)/usr/bin
	$(MKDIR) -p $(DSTROOT)/usr/share/man/man1
	ln -sf ../../$(Install_Prefix)/bin/clang $(DSTROOT)/usr/bin/clang
	cp $(DSTROOT)/$(Install_Prefix)/share/man/man1/clang.1 $(DSTROOT)/usr/share/man/man1/
	if [ -f $(DSTROOT)/$(Install_Prefix)/bin/clang++ ]; then \
	  ln -sf ../../$(Install_Prefix)/bin/clang++ $(DSTROOT)/usr/bin/clang++; \
	  ln -sf clang.1 $(DSTROOT)/$(Install_Prefix)/share/man/man1/clang++.1; \
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

install-clang-links:
	$(MKDIR) -p $(DSTROOT)/$(Install_Prefix)/bin
	ln -sf ../../../../../usr/bin/clang $(DSTROOT)/$(Install_Prefix)/bin/clang
ifeq ($(Clang_Enable_CXX), 1)
	ln -sf ../../../../../usr/bin/clang++ $(DSTROOT)/$(Install_Prefix)/bin/clang++
endif

install-lto-h:
	$(MKDIR) -p $(DSTROOT)/$(Install_Prefix)/local/include/llvm-c
	$(INSTALL_FILE) $(Sources)/include/llvm-c/lto.h $(DSTROOT)/$(Install_Prefix)/local/include/llvm-c

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
	done
	./merge-lipo `for arch in $(RC_ARCHS) ; do echo $(OBJROOT)/install-$$arch ; done` $(DSTROOT)
	$(_v) $(FIND) $(DSTROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(SYMROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(DSTROOT) -perm -0111 -type f -print | $(XARGS) -n 1 -P $(SYSCTL) dsymutil
	$(_v) cd $(DSTROOT) && find . -path \*.dSYM/\* -print | cpio -pdml $(SYMROOT)
	$(_v) find $(DSTROOT) -perm -0111 -type f -print | xargs -P $(SYSCTL) strip -S
	$(_v) find $(DSTROOT) -name \*.dSYM -print | xargs rm -r
	$(_v)- $(CHOWN) -R root:wheel $(DSTROOT) $(SYMROOT)

build-cross: configure-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building (Cross) for $$arch..." && \
		$(MAKE) -j$(SYSCTL) -C $(OBJROOT)/$$arch $(Build_Target) CFLAGS="-arch $$arch $(CFLAGS)" CXXFLAGS="-arch $$arch $(CXXFLAGS)" OPTIONAL_DIRS= || exit 1; \
	done

configure-cross: setup-tools-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Cross) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		$(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
			--host=arm-apple-darwin10 \
			--target=arm-apple-darwin10 \
			--build=i686-apple-darwin10 \
		  || exit 1 ; \
	done

# A cross-compiler configure will expect to find tools under names like
# arm-apple-darwin10-gcc, so make sure we have them. Note that -marm
# is added to the gcc/g++ command line due to rdar://7353031
setup-tools-cross:
	$(_v) $(MKDIR) $(OBJROOT)/bin
	$(_v) for prog in ar nm ranlib strip lipo ld as ; do \
	  echo '#!/bin/sh' > $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo 'exec '`xcrun -sdk $(SDKROOT) -find $$prog` '"$$@"' \
	    >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  chmod a+x $(OBJROOT)/bin/arm-apple-darwin10-$$prog || exit 1; \
	done
	$(_v) for prog in gcc g++ ; do \
	  echo '#!/bin/sh' > $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo "ARCH='-arch armv6'" >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo 'for i in $$@ ; do if [ "$$i" == "-arch" ] ; then ARCH= ; fi ; done' >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo 'exec '`xcrun -find $$prog` \
	    ' $$ARCH -isysroot '$(SDKROOT)' "$$@" -marm' \
	    >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  chmod a+x $(OBJROOT)/bin/arm-apple-darwin10-$$prog || exit 1 ; \
	done

###
# Debugging

# General debugging rule, use 'make dbg-print-XXX' to print the
# definition, value and origin of XXX.
make-print-%:
	$(error PRINT: $(value $*) = "$($*)" (from $(origin $*)))
