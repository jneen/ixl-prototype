BIN_DIR ?= ./bin
SRC_DIR = ./src
LIB_DIR ?= ./lib

HASKELL_OPTS +=

BUILD = dist/build/

BIN = $(BUILD)/ixl/ixl

CLEAN += $(BUILD)

.PHONY: all clean console run
all: $(BUILD)

run: $(BUILD)
	$(BIN)

clean:
	rm -rf $(CLEAN)

console:
	cabal-dev ghci

$(BUILD): Ixl.hs Main.hs $(shell find Ixl -name '*.hs')
	cabal-dev install
