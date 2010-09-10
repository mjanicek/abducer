#------------------------------------------------------------------------------#

# ICE

SLICE_PATH?=../slice

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# C++

BIN=abducer-server

CXX_HEADERS=CLI.h
CXX_HEADERS+=SliceToString.h TtyUtils.h EngineProtobufWrapper.h Logging.h Version.h common.h
CXX_HEADERS+=ForkingServer.h
CXX_HEADERS+=Constants.h
CXX_HEADERS+=weigabd.h
CXX_SOURCES+=SliceToProto.h ProtoToSlice.h ProtoUtil.h ProtocolException.h
CXX_HEADERS+=protocol.pb.h

CXX_SOURCES=ServerMain.cc CLI.cc
CXX_SOURCES+=SliceToString.cc TtyUtils.cc EngineProtobufWrapper.cc
CXX_SOURCES+=ForkingServer.cc
CXX_SOURCES+=weigabd.cc
CXX_SOURCES+=SliceToProto.cc ProtoToSlice.cc ProtoUtil.h ProtocolException.cc
CXX_SOURCES+=protocol.pb.cc

CXXFLAGS=-Wall
CXXFLAGS+=-I. -I$(ICE_HOME)/include
CXXFLAGS+=-I/opt/local/include
CXXFLAGS+=-g

LDFLAGS=-L$(ICE_HOME)/lib -lIce -lIceUtil $(ARCHFLAG)
LDFLAGS+=-L/opt/local/lib -lprotobuf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# Mercury

MBIN=abducer-pb
M_SOURCES=abducer-pb.m protobuf_runtime.m protocol_loading.m
M_SOURCES+=unix_socket.m
M_SOURCES+=protocol.m

LIBABDUCER=../lib/libabducer.a

MCFLAGS?=--search-lib-files-dir ../lib \
		--init-file ../lib/abducer.init \
		--link-object $(LIBABDUCER) \
		--cflag -g

#------------------------------------------------------------------------------#
# You shouldn't need to edit anything below.

GRADE?=$(shell mmc --output-grade-string)

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

ifeq ($(origin DEBUG), undefined)
else
MCFLAGS+=--trace-flag=debug
CXXFLAGS+=-DDEBUG
endif

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: compile
compile: $(BIN) $(MBIN)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

weigabd.h weigabd.cc: $(SLICE_PATH)/weigabd.ice
	$(ICE_HOME)/bin/slice2cpp -I$(SLICE_PATH) --source-ext cc $(SLICE_PATH)/weigabd.ice

%.pb.cc %.pb.h: %.proto
	protoc --cpp_out . $*.proto

%.m: %.proto
	mprotoc --out . $*.proto

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

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
	rm -rf $(OBJS) weigabd.cc weigabd.h \
		protocol.pb.cc protocol.pb.h \
		Mercury \
		$(addsuffix .err, $(basename $(M_SOURCES))) \
		$(addsuffix .mh, $(basename $(M_SOURCES))) \
		$(BIN) $(MBIN)
