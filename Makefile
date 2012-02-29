BIN_DIR ?= ./bin
SRC_DIR = ./src
LIB_DIR ?= ./lib

HASKELL_OPTS += -hide-package=mtl

IXL_SRC = $(SRC_DIR)/ixl.hs
IXL_BIN = $(BIN_DIR)/ixl

CLEAN += $(IXL_BIN)
CLEAN += $(LIB_DIR)/*

.PHONY: all
all: $(IXL_BIN)
.PHONY: clean
clean:
	rm -rf $(CLEAN)

$(IXL_BIN): $(IXL_SRC)
	haskell-compiler $(HASKELL_OPTS) $^ -o $@ -odir $(LIB_DIR) -hidir $(LIB_DIR)
