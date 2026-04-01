PROG=uq
# CC=clang
CC=gcc
LOG_LEVEL=5

SRC=check_parser.c parse_expression.c symbol_stuff.c type_stuff.c code_gen.c parser.c type_check.c main.c symbol_check.c type_registry.c


OBJ=$(SRC:.c=.o)

CFLAGS=-ggdb -std=c99 -DLOG_LEVEL=$(LOG_LEVEL)
LDFLAGS=-lm

all: build run

build: $(PROG)

$(PROG): $(OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)


%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

all_build:
	echo "Log level $(LOG_LEVEL)"
	$(CC) -ggdb -o $(PROG) $(SRC) -lm --std=c99 -DLOG_LEVEL=$(LOG_LEVEL)

clean:
	rm *.o

run:
	./$(PROG) main.gala
test:
	python3 tests.py

check_leaks:
	valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes -s ./uq main.gala
dbg:
	gdb --args ./uq main.gala
