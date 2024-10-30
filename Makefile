CC=clang
CFLAGS=-Wall -Wextra -Werror -Wno-unused-parameter -Wno-unused-variable -g

build:
	$(CC) $(CFLAGS) -o main main.c
