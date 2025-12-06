PROG=uq
all:
	./.clear.sh
	tcc -o $(PROG) new_main.c -lm
	./$(PROG) test.new_c
	# fasm builerplate.asm build/builerplate.o
	# fasm main.asm build/main.o
	# ld -o build/test_prog build/builerplate.o build/main.o
