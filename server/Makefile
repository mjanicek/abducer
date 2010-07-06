#------------------------------------------------------------------------------#

#ARCHFLAG=-arch i386

#------------------------------------------------------------------------------#

# ICE

SLICE_PATH?=../../../api-inference.v01/src/slice

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# C++

BIN=abducer-server

CXX_HEADERS=Tokens.h TermTokeniser.h Terms.h TtyUtils.h ForwardedAbducerServer.h SliceToString.h StringToSlice.h
CXX_HEADERS+=wabd.h

CXX_SOURCES=Tokens.cc TermTokeniser.cc ServerMain.cc Terms.cc TtyUtils.cc ForwardedAbducerServer.cc SliceToString.cc StringToSlice.cc
CXX_SOURCES+=wabd.cc

CXXFLAGS=-Wall
CXXFLAGS+=-I. -I$(ICE_HOME)/include
CXXFLAGS+=-DABDUCER_ICE_VERSION=\"$(ABDUCER_ICE_VERSION)\"
CXXFLAGS+=-g
CXXFLAGS+=$(ARCHFLAG)

LDFLAGS=-L$(ICE_HOME)/lib -lIce -lIceUtil $(ARCHFLAG)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# Mercury

MBIN=abducer-lfd
M_SOURCES=abducer-lfd.m protocol.m

LIBABDUCER=../lib/libabducer.a

MCFLAGS?=--search-lib-files-dir ../lib \
		--init-file ../lib/abducer.init \
		--link-object $(LIBABDUCER) \
		--cflag -g

#------------------------------------------------------------------------------#
# You shouldn't need to edit anything below.

#ifeq ($(shell uname -s), Darwin)
# on Darwin, Mercury doesn't support asm_fast (although it should for
# Intel-based systems?)
GRADE?=hlc.gc
#lse
# try asm_fast.gc otherwise
#RADE?=asm_fast.gc
#ndif

ifeq ($(origin ICE_HOME), undefined)
$(info )
$(info Please set the environment variable ICE_HOME to the root of your Ice)
$(info installation.)
$(info e.g. export ICE_HOME=/opt/local)
$(info )
$(error ICE_HOME undefined)
endif

# on BSD, without deactivating suffix rules, any "non-suffixed"
# target would be matched by some sort of default rule (using
# $(CC)), not by our rule.
.SUFFIXES:

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

ABDUCER_ICE_VERSION:=$(shell ./svn-file-version $(SLICE_PATH)/wabd.ice)

ifeq ($(origin DEBUG), undefined)
else
MCFLAGS+=--trace-flag=debug
CXXFLAGS+=-DDEBUG
endif

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: compile
compile: $(BIN) $(MBIN)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

wabd.h wabd.cc: $(SLICE_PATH)/wabd.ice
	$(ICE_HOME)/bin/slice2cpp -I$(SLICE_PATH) --source-ext cc $(SLICE_PATH)/wabd.ice

%.o: %.cc $(CXX_HEADERS)
	$(CXX) $(CXXFLAGS) -c $*.cc

OBJS=$(addsuffix .o, $(basename $(CXX_SOURCES)))

$(BIN): $(CXX_HEADERS) $(OBJS)
	$(CXX) $(LDFLAGS) $(OBJS) -o $@

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

$(MBIN): $(M_SOURCES) $(LIBABDUCER)
	mmc --make \
		--grade $(GRADE) \
		$(MCFLAGS) \
		$@

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: clean
clean:
	rm -rf $(OBJS) wabd.cc wabd.h \
		Mercury \
		$(addsuffix .err, $(basename $(M_SOURCES))) \
		$(addsuffix .mh, $(basename $(M_SOURCES))) \
		$(BIN) $(MBIN)
