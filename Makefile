PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)

CC=clang
CFLAGS=-Wall -Wextra -Werror -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -g

all: build

build:
	$(CC) $(CFLAGS) -o main main.c
	./main stack.sl -a > slc.asm
	fasm slc.asm
	ld slc.o -o slc

.PHONY: clean install

install:
	mkdir -p $(BINDIR)
	mkdir -p $(LIBDIR)
	cp main $(BINDIR)/slc
	cp -r lib $(LIBDIR)

clean:
	rm -rf main slc main.o slc.o main.asm slc.asm
