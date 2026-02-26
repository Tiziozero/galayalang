PROG=uq
# CC=clang
CC=clang
LLVM_CFLAGS=$(shell llvm-config --cflags)
LLVM_LDFLAGS=$(shell llvm-config --ldflags --libs core executionengine mcjit native target --system-libs)
LOG_LEVEL=5;

# pacman -S llvm clang lldb llvm-libs
all: build run
build:
	echo "Log level $(LOG_LEVEL)"
	$(CC) -ggdb -o $(PROG) *.c -lm --std=c99 -DLOG_LEVEL=$(LOG_LEVEL)

run:
	./$(PROG) main.gala

check_leaks:
	valgrind --leak-check=full --track-origins=yes ./uq main.gala
