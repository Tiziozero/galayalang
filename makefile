PROG=uq
all:
	./.clear.sh
	cc -g -o $(PROG) newmain2.c -lm --std=c99
	./$(PROG) test.new_c

build_vm: 
	cc -o vm vm.c
	./vm a.out
