PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)
OUTDIR=out
EXT=sl
EXAMPLES_DIR=examples
CC=$(OUTDIR)/slc

EXAMPLE_FILES := $(wildcard $(EXAMPLES_DIR)/*.$(EXT))
EXAMPLE_OUT_FILES := $(patsubst $(EXAMPLES_DIR)/%.$(EXT), $(OUTDIR)/%, $(EXAMPLE_FILES))

all: build

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

$(OUTDIR)/%: $(EXAMPLES_DIR)/%.$(EXT)
	$(CC) $< > $(OUTDIR)/%.asm
	fasm $(OUTDIR)/%.asm $(OUTDIR)/%.o
	ld $(OUTDIR)/%.o -o $@

examples: $(EXAMPLE_OUT_FILES)

.PHONY: clean install bootstrap examples

install:
	mkdir -p $(BINDIR)
	mkdir -p $(LIBDIR)
	cp $(OUTDIR)/slc $(BINDIR)/slc
	cp -r lib $(LIBDIR)

clean:
	rm -rf $(OUTDIR)
