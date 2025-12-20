PROG=uq
CC=clang
LLVM_CFLAGS=$(shell llvm-config --cflags)
LLVM_LDFLAGS=$(shell llvm-config --ldflags --libs core executionengine mcjit native target --system-libs)

all: code_gen_lib
	# ./.clear.sh
	$(CC) -g -o $(PROG) main.c parser.c -L. -lcodegen -lm --std=c99 $(LLVM_LDFLAGS)
	./$(PROG) main.gala

code_gen_lib: code_gen.o
	ar rcs libcodegen.a code_gen.o

code_gen.o: code_gen.c
	$(CC) -c -g code_gen.c -o code_gen.o --std=c99 $(LLVM_CFLAGS)

clean:
	rm -f *.o *.a $(PROG)

.PHONY: all clean code_gen_lib

build_vm: 
	cc -o vm vm.c
	./vm main.gala
