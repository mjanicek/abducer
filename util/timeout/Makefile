.PHONY: compile
compile: timeout

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

CFLAGS=-Wall -ansi -pedantic -pthread
LIBS=-pthread

C_HEADERS=sig2str.h intprops.h

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

timeout: timeout.o sig2str.o
	$(CC) $(LIBS) -o timeout $^

%.o: %.c $(C_HEADERS)
	$(CC) $(CFLAGS) -c $<

.PHONY: clean
clean:
	rm -f timeout timeout.o sig2str.o
