
CC = gcc
CFLAGS = -Wall -g

yoctolisp : yoctolisp.c
	$(CC) $(CFLAGS) $< -o $@

