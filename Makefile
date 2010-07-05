.PHONY: main
main: server-mk2

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: lib
lib:
	make -C lib

.PHONY: server-mk1
server-mk1: lib
	make -C server-mk1
	[ -e abducer-server-mk1-bin ] || ln -s server-mk1/abducer-server-bin

.PHONY: server-mk2
server-mk2: lib
	make -C server-mk2
	[ -e abducer-server-mk2-bin ] || ln -s server-mk2/abducer-server-mk2 abducer-server-mk2-bin
	[ -e abducer-server-mk2-core ] || ln -s server-mk2/abducer-lfd abducer-server-mk2-core

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
	make -C server-mk1 clean
	make -C server-mk2 clean
	make -C tests clean
	rm -f abducer-server-mk1-bin
	rm -f abducer-server-mk2-bin abducer-server-mk2-core
	rm -f abducer-cli check-file
