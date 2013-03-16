BIN_DIR ?= ./bin
SRC_DIR = ./src
LIB_DIR ?= ./lib

PREFIX ?= $(HOME)/.local

CABAL ?= $(shell which cabal)

BUILD = dist/build/

BIN = $(BUILD)/ixl/ixl

CLEAN += $(BUILD)

.PHONY: all clean test install
all: $(BUILD)

clean:
	rm -rf $(CLEAN)

test: $(BUILD)
	$(CABAL) test

$(BUILD): Ixl.hs Main.hs $(shell find Ixl -name '*.hs')
	$(CABAL) build

install: $(BUILD)
	install -m 0755 $(BIN) $(PREFIX)/bin/ixl
	install -m 0755 -d $(PREFIX)/lib/ixl
	install -m 0644 $(BUILD)/Ixl/*.o -t $(PREFIX)/lib/ixl
