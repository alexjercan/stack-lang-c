PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)
OUTDIR=out

CC=clang
CFLAGS=-Wall -Wextra -Werror -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -g

all: build

$(OUTDIR):
	mkdir -p $(OUTDIR)

bootstrap: $(OUTDIR)
	fasm bootstrap/stack.asm $(OUTDIR)/stack.o
	ld $(OUTDIR)/stack.o -o $(OUTDIR)/slc

build: bootstrap
	./slc stack.sl -a > bootstrap/stack.asm
	fasm bootstrap/stack.asm $(OUTDIR)/stack.o
	ld $(OUTDIR)/stack.o -o slc

.PHONY: clean install bootstrap

install:
	mkdir -p $(BINDIR)
	mkdir -p $(LIBDIR)
	cp $(OUTDIR)/slc $(BINDIR)/slc
	cp -r lib $(LIBDIR)

clean:
	rm -rf slc ./bootstrap/stack.o
