PREFIX=/usr/local
BINDIR=$(PREFIX)/bin

CC=clang
CFLAGS=-Wall -Wextra -Werror -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -g

all: build

build:
	$(CC) $(CFLAGS) -o main main.c

.PHONY: clean install

install:
	mkdir -p $(BINDIR)
	cp main $(BINDIR)/slc

clean:
	rm -rf main
