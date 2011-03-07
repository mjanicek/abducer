# Binary files to be built.
#
MLIB=libabducer

# Mercury source files.
#
MFILES=abduction.m context.m lang.m assumability.m modality.m stringable.m ctx_modality.m lang_io.m ctx_io.m lang_ops.m ctx_modality.m ctx_loadable.m enumerable.m utils.m ctx_loadable_io.m loading.m blacklist.m anytime.m prob.m tty.m check.m abducer.m

# Mercury compiler flags
#
MCFLAGS?=

#------------------------------------------------------------------------------#
# You shouldn't need to edit anything below.

GRADE?=$(shell mmc --output-grade-string)

ifeq ($(origin DEBUG), undefined)
else
MCFLAGS+=--trace-flag=debug
MCFLAGS+=--cflag -g
endif

ifeq ($(origin PROF), undefined)
else
MCFLAGS+=--profiling --memory-profiling --time-profiling
endif

MCFLAGS+=--warn-unused-imports
MCFLAGS+=--cflag -D_GNU_SOURCE

# on BSD, without deactivating suffix rules, any "non-suffixed"
# target would be matched by some sort of default rule (using
# $(CC)), not by our rule.
.SUFFIXES:

.PHONY: compile
compile: $(MLIB)

$(MLIB): $(MFILES)
	mmc --make \
		--link-shared-lib-command=$(CXX) \
		--lib-linkage static \
		--grade $(GRADE) \
		--libgrade $(GRADE) \
		$(MCFLAGS) \
		$(LDFLAGS) \
		$@

.PHONY: clean
clean:
	rm -rf $(BIN) Mercury \
		$(addsuffix .err, $(basename $(MFILES))) \
		$(addsuffix .mh, $(basename $(MFILES))) \
		libabducer.a abducer.init
