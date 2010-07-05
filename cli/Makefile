# $Id: Makefile 4590 2009-11-13 15:21:40Z janicek $

# Binary files to be built.
#
MBIN=check-file abducer-cli

# Mercury source files.
#
MFILES=check-file.m abducer-cli.m

LIBABDUCER=../lib/libabducer.a

# Mercury compiler flags
#
MCFLAGS?=--search-lib-files-dir ../lib \
		--init-file ../lib/abducer.init \
		--link-object $(LIBABDUCER)

#------------------------------------------------------------------------------#
# You shouldn't need to edit anything below.

#ifeq ($(shell uname -s), Darwin)
# on Darwin, Mercury doesn't support asm_fast (although it should for
# Intel-based systems?)
GRADE?=hlc.gc
#else
# try asm_fast.gc otherwise
#GRADE?=asm_fast.gc
#endif

# on BSD, without deactivating suffix rules, any "non-suffixed"
# target would be matched by some sort of default rule (using
# $(CC)), not by our rule.
.SUFFIXES:

.PHONY: compile
compile: $(MBIN)

$(MBIN): $(MFILES) $(LIBABDUCER)
	mmc --make \
		--grade $(GRADE) \
		$(MCFLAGS) \
		$@

.PHONY: clean
clean:
	rm -rf $(BIN) Mercury \
		$(addsuffix .err, $(basename $(MFILES))) \
		$(addsuffix .mh, $(basename $(MFILES))) \
		$(MBIN)
