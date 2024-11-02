CC=clang
CFLAGS=-Wall -Wextra -Werror -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -g

build:
	$(CC) $(CFLAGS) -o main main.c

.PHONY: clean

clean:
	rm -rf main
