
CC = gcc
CFLAGS = -Wall -g

yoctolisp : yoctolisp.c
	$(CC) $(CFLAGS) $< -o $@

yoctolisp.html : yoctolisp.md
	markdown $< > $@

