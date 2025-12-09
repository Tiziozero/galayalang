PROG=uq
all:
	./.clear.sh
	cc -g -o $(PROG) new_main.c -lm --std=c99
	./$(PROG) test.new_c
	# fasm builerplate.asm build/builerplate.o
	# fasm main.asm build/main.o
	# ld -o build/test_prog build/builerplate.o build/main.o
