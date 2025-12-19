PROG=uq
all:
	./.clear.sh
	cc -g -o $(PROG) main.c -lm --std=c99
	./$(PROG) main.gala

build_vm: 
	cc -o vm vm.c
	./vm main.gala
