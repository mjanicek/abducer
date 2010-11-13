.PHONY: all
all: server cli tests

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: lib
lib:
	make -C lib

ABDUCTION_ENGINE=abduction-engine
SERVER_BIN=abducer-server-bin

.PHONY: server
server: lib
	make -C server
	[ -e $(SERVER_BIN) ] || ln -s server/abducer-server $(SERVER_BIN)
	[ -e $(ABDUCTION_ENGINE) ] || ln -s server/abducer-pb $(ABDUCTION_ENGINE)

.PHONY: cli
cli: lib
	make -C cli
	[ -e check-abd ] || ln -s cli/check-abd
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
	rm -f $(SERVER_BIN) $(ABDUCTION_ENGINE)
	rm -f abducer-cli check-abd
	rm -f timeout
