#------------------------------------------------------------------------------#
# Build settings

OUTPUT_DIR?=../bin

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# ICE

SLICE_PATH?=../slice

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# C++

BIN=$(OUTPUT_DIR)/abducer-server

CXX_HEADERS=CLI.h
CXX_HEADERS+=SliceToString.h EngineProtobufWrapper.h Version.h common.h
CXX_HEADERS+=BoundUnixSocket.h
CXX_HEADERS+=ForkingServer.h
CXX_HEADERS+=Constants.h
CXX_HEADERS+=abducer.h
CXX_SOURCES+=SliceToProto.h ProtoToSlice.h ProtoUtil.h ProtocolException.h
CXX_HEADERS+=protocol.pb.h

CXX_SOURCES=ServerMain.cc CLI.cc
CXX_SOURCES+=SliceToString.cc EngineProtobufWrapper.cc
CXX_SOURCES+=BoundUnixSocket.cc
CXX_SOURCES+=ForkingServer.cc
CXX_SOURCES+=abducer.cc
CXX_SOURCES+=SliceToProto.cc ProtoToSlice.cc ProtoUtil.h ProtocolException.cc
CXX_SOURCES+=protocol.pb.cc

#  -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   #

CXXFLAGS=-Wall
CXXFLAGS+=-I. -I$(ICE_HOME)/include
CXXFLAGS+=-g

PROTOBUF_CXXFLAGS=$(shell pkg-config --cflags protobuf 2>/dev/null || true)

ifneq "$(strip $(PROTOBUF_CXXFLAGS))" ""
CXXFLAGS+=$(PROTOBUF_CXXFLAGS)
else
$(warning pkg-config does not know protobuf, assuming default CXXFLAGS)
CXXFLAGS+=-pthread
endif

CXXFLAGS+=$(shell pkg-config --cflags liblog4cxx)

#  -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   #

LDFLAGS=-L$(ICE_HOME)/lib -lIce -lIceUtil $(ARCHFLAG)
LDFLAGS+=-lprotobuf

PROTOBUF_LDFLAGS=$(shell pkg-config --libs protobuf 2>/dev/null || true)

ifneq "$(strip $(PROTOBUF_LDFLAGS))" ""
LDFLAGS+=$(PROTOBUF_LDFLAGS)
else
$(warning pkg-config does not know protobuf, assuming default LDFLAGS)
LDFLAGS+=-lprotobuf
endif

LDFLAGS+=$(shell pkg-config --libs liblog4cxx)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Mercury

MBIN=$(OUTPUT_DIR)/abducer-engine-pb
M_SOURCES=abducer-engine-pb.m protobuf_runtime.m protocol_loading.m
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
$(warning ICE_HOME undefined)
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

abducer.h abducer.cc: $(SLICE_PATH)/abducer.ice
	$(ICE_HOME)/bin/slice2cpp -I$(SLICE_PATH) --source-ext cc $(SLICE_PATH)/abducer.ice

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
		`basename $@`
	mv `basename $(MBIN)` $(OUTPUT_DIR) || true

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: clean
clean:
	rm -rf $(OBJS) abducer.cc abducer.h \
		protocol.pb.cc protocol.pb.h \
		Mercury \
		$(addsuffix .err, $(basename $(M_SOURCES))) \
		$(addsuffix .mh, $(basename $(M_SOURCES))) \
