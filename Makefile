SERVER_BIN=abducer-server
ENGINE_BIN=abducer-engine-pb

#------------------------------------------------------------------------------#

.PHONY: all
all: server java tests java

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: lib
lib:
	make -C lib

.PHONY: server
server: lib
	make -C server OUTPUT_DIR=`pwd`/bin

.PHONY: cli
cli: lib
	make -C cli OUTPUT_DIR=`pwd`/bin

.PHONY: tests
tests: lib util
	make -C tests

.PHONY: java
java:
	cd api-java && ant

.PHONY: util
util:
	make -C util
	[ -e bin/timeout ] || cp util/timeout/timeout bin

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

.PHONY: clean
clean:
	make -C lib clean
	make -C server clean
	make -C cli clean
	make -C tests clean
	make -C util clean
	cd api-java && ant clean
	rm -f bin/$(SERVER_BIN) bin/$(ENGINE_BIN)
	rm -f bin/abducer-cli bin/check-abd
	rm -f bin/timeout
	rm -r doc/javadoc
