PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)
OUTDIR=out
EXT=stack
EXAMPLES_DIR=examples
CC=$(OUTDIR)/slc
SLC=$(CC)
PYTHON=python3

EXAMPLE_FILES := $(wildcard $(EXAMPLES_DIR)/*.$(EXT))
EXAMPLE_OUT_FILES := $(patsubst $(EXAMPLES_DIR)/%.$(EXT), $(OUTDIR)/%, $(EXAMPLE_FILES))

all: build stackls

$(OUTDIR):
	mkdir -p $(OUTDIR)

$(CC): $(OUTDIR)
	fasm bootstrap/stack.asm $(OUTDIR)/stack.o
	ld $(OUTDIR)/stack.o -o $(CC)

bootstrap: $(CC)

build: $(CC)
	$(CC) stack.$(EXT) -a > bootstrap/stack.asm
	fasm bootstrap/stack.asm $(OUTDIR)/stack.o
	ld $(OUTDIR)/stack.o -o $(CC)

$(OUTDIR)/%: $(EXAMPLES_DIR)/%.$(EXT) build
	$(CC) $< -o $@ $(LINKFLAGS)

# Link flags of one example. Make runs pkg-config only for that example.
$(OUTDIR)/11-raylib: LINKFLAGS = -- $(shell pkg-config --libs raylib)

examples: $(EXAMPLE_OUT_FILES)

ifeq ($(abspath $(SLC)),$(abspath $(CC)))
STACKLS_DEPS=build
else
STACKLS_DEPS=$(SLC)
endif

stackls: $(STACKLS_DEPS)
	mkdir -p $(OUTDIR)
	STACK_HOME=. $(SLC) stackls.$(EXT) -a > $(OUTDIR)/stackls.asm
	fasm $(OUTDIR)/stackls.asm $(OUTDIR)/stackls.o
	ld $(OUTDIR)/stackls.o -o $(OUTDIR)/stackls

test-lsp: stackls
	$(PYTHON) tests/lsp/protocol.py $(OUTDIR)/stackls

.PHONY: build clean install install-slc install-stackls bootstrap examples stackls test-lsp

install: install-slc install-stackls

install-slc: build
	mkdir -p $(BINDIR)
	mkdir -p $(LIBDIR)
	cp $(OUTDIR)/slc $(BINDIR)/slc
	cp -r lib $(LIBDIR)

install-stackls: stackls
	mkdir -p $(BINDIR)
	cp $(OUTDIR)/stackls $(BINDIR)/stackls

clean:
	rm -rf $(OUTDIR)
