#------------------------------------------------------------------------------#
# Build settings

OUTPUT_DIR?=../bin

# Binary files to be built.
#
MBIN=check-abd abducer-cli

# Mercury source files.
#
MFILES=check-abd.m abducer-cli.m

LIBABDUCER=../lib/libabducer.a

# Mercury compiler flags
#
MCFLAGS?=--search-lib-files-dir ../lib \
		--init-file ../lib/abducer.init \
		--link-object $(LIBABDUCER)

#------------------------------------------------------------------------------#
# You shouldn't need to edit anything below.

GRADE?=$(shell mmc --output-grade-string)

ifeq ($(origin PROF), undefined)
else
MCFLAGS+=--profiling --memory-profiling --time-profiling
endif

# on BSD, without deactivating suffix rules, any "non-suffixed"
# target would be matched by some sort of default rule (using
# $(CC)), not by our rule.
.SUFFIXES:

.PHONY: compile
compile: $(addprefix $(OUTPUT_DIR)/, $(MBIN))

$(addprefix $(OUTPUT_DIR)/, $(MBIN)): $(MFILES) $(LIBABDUCER)
	mmc --make \
		--grade $(GRADE) \
		$(MCFLAGS) \
		$(MBIN)
	mv abducer-cli $(OUTPUT_DIR) || true
	mv check-abd $(OUTPUT_DIR) || true

.PHONY: clean
clean:
	rm -rf Mercury \
		$(addsuffix .err, $(basename $(MFILES))) \
		$(addsuffix .mh, $(basename $(MFILES))) \
