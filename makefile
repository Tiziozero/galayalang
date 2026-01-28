PROG=uq
# CC=clang
CC=clang
LLVM_CFLAGS=$(shell llvm-config --cflags)
LLVM_LDFLAGS=$(shell llvm-config --ldflags --libs core executionengine mcjit native target --system-libs)

# pacman -S llvm clang lldb llvm-libs
all: build run
build:
	$(CC) -ggdb -o $(PROG) type_helpers.c parser_helpers.c printers_parser.c type_checker.c symbol_check.c main.c parser.c code_gen.c -lm --std=c99 
run: build
	./$(PROG) main.gala

print_ast:
	cc -o test ast_to_json.c -lm parser.c code_gen.c && ./test
code_gen_lib: code_gen.o
	# ./.clear.sh
	#  $(LLVM_LDFLAGS)
	ar rcs libcode_gen.a code_gen.o

code_gen.o: code_gen.c
	$(CC) -c -g code_gen.c -o code_gen.o --std=c99 $(LLVM_CFLAGS)

clean:
	rm -f *.o *.a $(PROG)

.PHONY: all clean code_gen_lib

build_vm: 
	cc -o vm vm.c
	./vm main.gala
