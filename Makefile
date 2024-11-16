PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(BINDIR)/lib

CC=clang
CFLAGS=-Wall -Wextra -Werror -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -g

all: build

build:
	$(CC) $(CFLAGS) -o main main.c

.PHONY: clean install

install:
	mkdir -p $(BINDIR)
	mkdir -p $(LIBDIR)
	cp main $(BINDIR)/slc
	cp lib $(LIBDIR)

clean:
	rm -rf main
