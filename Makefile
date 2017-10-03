# Main makefile storing common options for building all libraries, by triggering
# individual makefiles from each library directory
#--------------------------------------------------------------------------------

# The intention is that the user points "make" at one of the platform specific
# files, which include this file at the end; here we try to ensure that this
# has happened (if it hasn't, various important variables will not be set)
ifndef PLATFORM
$(error Platform file not loaded, re-run as "make -f" providing a file from \
the "make" subdirectory as an argument)
endif

# Setup destination directory - this can be overidden by the user if they wish
# to install directly to a different location
LIBDIR_ROOT ?= ${PWD}/build
LIBDIR_OUT ?= ${LIBDIR_ROOT}/${PLATFORM}
export

# Setup the flags which will be passed to all compilations - add the openMP
# flags based on the setting below (defaults to true)
SHUM_OPENMP ?= true
ifeq (${SHUM_OPENMP}, true)
FCFLAGS=${FCFLAGS_PREC} ${FCFLAGS_EXTRA} ${FCFLAGS_OPENMP}
CCFLAGS=${CCFLAGS_PREC} ${CCFLAGS_EXTRA} ${CCFLAGS_OPENMP}
else ifeq (${SHUM_OPENMP}, false)
FCFLAGS=${FCFLAGS_PREC} ${FCFLAGS_EXTRA} ${FCFLAGS_NOOPENMP}
CCFLAGS=${CCFLAGS_PREC} ${CCFLAGS_EXTRA} ${CCFLAGS_NOOPENMP}
else
$(error If specified in the environment SHUM_OPENMP environment variable must \
be set to either "true" or "false")
endif

# Default target - build all available libraries
#--------------------------------------------------------------------------------
.PHONY: default
default: libs

# Output directories
#--------------------------------------------------------------------------------
OUTDIRS=${LIBDIR_OUT}/lib ${LIBDIR_OUT}/include 
${OUTDIRS}:
	mkdir -p ${LIBDIR_OUT}/lib
	mkdir -p ${LIBDIR_OUT}/include

OUTDIR_TESTS=${LIBDIR_OUT}/tests

${OUTDIR_TESTS}:
	mkdir -p ${LIBDIR_OUT}/tests

# Setup path to directory containing common/shared components; these include
# functions that provide Shumlib version information and the C Precision Bomb
# (which will protect against compilation on platforms where the assumptions 
# about precision made in the libraries is invalid)
#--------------------------------------------------------------------------------
COMMON_DIR=${PWD}/common

# The FRUIT source itself
#------------------------
FRUIT=fruit
.PHONY: ${FRUIT}
${FRUIT}: ${OUTDIRS}
	${MAKE} -C ${FRUIT}

# Libraries
#--------------------------------------------------------------------------------

# String conv
#------------
STR_CONV=shum_string_conv
${STR_CONV}: ${OUTDIRS}
	${MAKE} -C ${STR_CONV}/src
${STR_CONV}_tests: fruit ${STR_CONV}
	${MAKE} -C ${STR_CONV}/test

# Byte-swapping
#--------------
BSWAP=shum_byteswap
${BSWAP}: ${STR_CONV} ${OUTDIRS}
	${MAKE} -C ${BSWAP}/src

# Data conv
#----------
DATA_CONV=shum_data_conv
${DATA_CONV}: ${STR_CONV} ${OUTDIRS}
	${MAKE} -C ${DATA_CONV}/src

# WGDOS packing
#--------------
PACK=shum_wgdos_packing
${PACK}: ${STR_CONV} ${OUTDIRS}
	${MAKE} -C ${PACK}/src

# Thread Utils
#--------------
THREAD_UTILS=shum_thread_utils
${THREAD_UTILS}: ${OUTDIRS}
	${MAKE} -C ${THREAD_UTILS}/src

# All libs targets
#--------------
ALL_LIBS=${BSWAP} ${STR_CONV} ${DATA_CONV} ${PACK} ${THREAD_UTILS}

# auto-generate test targets
$(wildcard $(addsuffix /test, ${ALL_LIBS})): %/test: ${FRUIT} %
	${MAKE} -C $@

$(addsuffix _tests, ${ALL_LIBS}): %_tests: ${OUTDIR_TESTS} %/test
	${MAKE} test_generic

.PHONY: libs ${ALL_LIBS} $(wildcard $(addsuffix /test, ${ALL_LIBS})) $(addsuffix _tests, ${ALL_LIBS})

# Add a target which points to all libraries
libs: ${ALL_LIBS}

# FRUIT testing control
#--------------------------------------------------------------------------------

.PHONY: test test_generic check

check: test

test: ${OUTDIR_TESTS} $(wildcard $(addsuffix /test, ${ALL_LIBS}))
	${MAKE} test_generic

test_generic:
	${MAKE} -f ${FRUIT}/Makefile-driver
	${LIBDIR_OUT}/tests/fruit_tests_static.exe
	${LIBDIR_OUT}/tests/fruit_tests_dynamic.exe

# Cleanup targets
#--------------------------------------------------------------------------------
.PHONY: clean clean-temp clean-build
clean-temp:
	@$(foreach libname,$(ALL_LIBS),${MAKE} -C $(libname)/src clean;)
	@$(foreach libname_test,$(wildcard $(addsuffix /test, ${ALL_LIBS})),${MAKE} -C $(libname_test) clean;)
	${MAKE} -C ${FRUIT} clean
	${MAKE} -f ${FRUIT}/Makefile-driver clean

clean-build:
	rm -rf ${OUTDIRS} ${OUTDIR_TESTS}
	rm -rf ${LIBDIR_OUT}
	rmdir ${LIBDIR_ROOT} || :

clean: clean-temp clean-build
