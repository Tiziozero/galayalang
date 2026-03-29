PROG=uq
# CC=clang
CC=clang
LOG_LEVEL=5;

# pacman -S llvm clang lldb llvm-libs
all: build run
build:
	echo "Log level $(LOG_LEVEL)"
	$(CC) -ggdb -o $(PROG) *.c -lm --std=c99 -DLOG_LEVEL=$(LOG_LEVEL)

run:
	./$(PROG) main.gala
test:
	python3 tests.py

check_leaks:
	valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes -s ./uq main.gala
dbg:
	gdb --args ./uq main.gala
