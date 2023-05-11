UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	format=macho64
	CC=arch -x86_64 gcc
else
	format=elf64
	CC=gcc
endif

.PHONY: default clean test

objs = \
	main.o \
	values.o \
	print.o \
	io.o

default: submit.zip

submit.zip:
	zip submit.zip -r * -x \*.[os] -x \*~ -x "compiled/*" -x \*zip

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

%.run: %.o runtime.o
	$(CC) runtime.o $< -o $@

.c.o:
	$(CC) -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	-rm *.o *.s *.run

test: example.run
	@test "$(shell ./example.run)" = "$(shell racket example.rkt)"
