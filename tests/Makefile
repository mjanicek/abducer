# $Id: Makefile 4590 2009-11-13 15:21:40Z janicek $

# Binary files to be built.
#
MBIN=test_abduction test_formulae test_lf test_modality test_rcc test_abd_bm

# Mercury source files.
#
MFILES=test_abduction.m test_formulae.m test_lf.m test_modality.m test_rcc.m test_abd_bm.m

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
