galac: *.odin
	odin build . -out:galac
all: galac
	galac main.gala -o prog && ./prog
.PHONY: all
