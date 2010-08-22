.PHONY: all
all: server cli tests

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: lib
lib:
	make -C lib

.PHONY: server
server: lib
	make -C server
	[ -e abducer-server-bin ] || ln -s server/abducer-server abducer-server-bin
	[ -e abducer-server-core ] || ln -s server/abducer-lfd abducer-server-core

.PHONY: cli
cli: lib
	make -C cli
	[ -e check-file ] || ln -s cli/check-file
	[ -e abducer-cli ] || ln -s cli/abducer-cli

.PHONY: tests
tests: lib
	make -C tests

.PHONY: clean
clean:
	make -C lib clean
	make -C server clean
	make -C tests clean
	rm -f abducer-server-bin abducer-server-core
	rm -f abducer-cli check-file
