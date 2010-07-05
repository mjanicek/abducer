# $Id: Makefile 3675 2009-10-16 13:39:23Z janicek $

# Binary files to be built.
#
BIN=abducer-server-bin

# Mercury source files.
#
MFILES=abducer-server-bin.m MercuryAbducerServer_mint.m TypeConversions_mint.m

LIBABDUCER=../lib/libabducer.a

# Mercury compiler flags
#
MCFLAGS?=--search-lib-files-dir ../lib \
		--init-file ../lib/abducer.init \
		--link-object $(LIBABDUCER)

#------------------------------------------------------------------------------#

SLICE_PATH?=../../../slice

#------------------------------------------------------------------------------#
# You shouldn't need to edit anything below.

ifeq ($(origin ICE_HOME), undefined)
$(info )
$(info Please set the environment variable ICE_HOME to the root of your Ice)
$(info installation.)
$(info e.g. export ICE_HOME=/opt/local)
$(info )
$(error ICE_HOME undefined)
endif
#ICE_HOME=$(HOME)/Developer/Ice-3.3.1

ifeq ($(origin MERCURY_HOME), undefined)
$(info )
$(info Please set the variable MERCURY_HOME to the root of your Mercury installation.)
$(info e.g. export MERCURY_HOME=/usr/local/mercury-rotd-2008-11-30)
$(info )
$(error MERCURY_HOME undefined)
endif
#MERCURY_HOME=$(HOME)/Developer/mercury-rotd-2008-11-30

CXXFLAGS=-Wall
LDFLAGS=-L$(ICE_HOME)/lib -lIce -lIceUtil --runtime-flags '--nondetstack-size 100000'

ifeq ($(shell uname -s), Darwin)
# on Darwin, Mercury doesn't support asm_fast (although it should for
# Intel-based systems?)
GRADE?=hlc.gc
else
# try asm_fast.gc otherwise
GRADE?=asm_fast.gc
endif

ifeq ($(origin DEBUG), undefined)
else
MCFLAGS+=--trace-flag=debug
MCFLAGS+=--cflag -g
CXXFLAGS+=-DDEBUG
CXXFLAGS+=-g
endif

ifeq ($(origin PROF), undefined)
else
MCFLAGS+=--profiling --memory-profiling --time-profiling
endif

# TODO: this just returns the revision of the directory the slice file
# is in.
ABDUCER_ICE_VERSION:=$(shell ./svn-file-version $(SLICE_PATH)/Abducer.ice)

# on BSD, without deactivating suffix rules, any "non-suffixed"
# target would be matched by some sort of default rule (using
# $(CC)), not by our rule.
.SUFFIXES:

.PHONY: compile
compile: $(BIN)

.PRECIOUS: %.o
%.o: %.cc aserv.h Abducer.h TypeConversions.h TypeConversions_mint.mh MercuryAbducerServer.h MercuryAbducerServer_mint.mh TtyUtils.h
	$(CXX) $(CXXFLAGS) -I. -I$(ICE_HOME)/include -I$(MERCURY_HOME)/lib/mercury/inc -I$(MERCURY_HOME)/lib/mercury/conf -DABDUCER_ICE_VERSION=\"$(ABDUCER_ICE_VERSION)\" -c $*.cc

Abducer.h Abducer.cc: $(SLICE_PATH)/Abducer.ice
	slice2cpp -I$(SLICE_PATH) --source-ext cc $(SLICE_PATH)/Abducer.ice

abducer-server-bin: $(MFILES) aserv.o Abducer.o TypeConversions.o MercuryAbducerServer.o TtyUtils.o $(LIBABDUCER)
	mmc --make \
		--link-executable-command=$(CXX) \
		--link-object Abducer.o \
		--link-object TypeConversions.o \
		--link-object MercuryAbducerServer.o \
		--link-object TtyUtils.o \
		--link-object aserv.o \
		--grade $(GRADE) \
		$(MCFLAGS) \
		$(LDFLAGS) \
		$@

.PRECIOUS: %.mh
%.mh: %.m
	mmc --make \
		--grade $(GRADE) \
		$(MCFLAGS) \
		$*.o

.PHONY: clean
clean:
	rm -rf $(BIN) Mercury \
		$(addsuffix .err, $(basename $(MFILES))) \
		$(addsuffix .mh, $(basename $(MFILES))) \
		abducer-server \
		TypeConversions.o MercuryAbducerServer.o TtyUtils.o \
		Abducer.o Abducer.cc Abducer.h
