BIN_DIR ?= ./bin
SRC_DIR = ./src
LIB_DIR ?= ./lib

HASKELL_OPTS +=

BUILD = build/

CLEAN += $(BUILD)

.PHONY: all clean console run
all: $(BUILD)

clean:
	rm -rf $(CLEAN)

console:
	cabal-dev ghci

$(BUILD): Ixl/
	cabal-dev install
