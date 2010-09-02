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
	[ -e abducer-server-core ] || ln -s server/abducer-pb abducer-server-core

.PHONY: cli
cli: lib
	make -C cli
	[ -e check-file ] || ln -s cli/check-file
	[ -e abducer-cli ] || ln -s cli/abducer-cli

.PHONY: tests
tests: lib util
	make -C tests

.PHONY: util
util:
	make -C util
	[ -e timeout ] || ln -s util/timeout/timeout

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: clean
clean:
	make -C lib clean
	make -C server clean
	make -C cli clean
	make -C tests clean
	make -C util clean
	rm -f abducer-server-bin abducer-server-core
	rm -f abducer-cli check-file
	rm -f timeout
