all: galac
	./galac main.gala -o:prog
	./prog

galac: *.odin
	odin build . -out:galac -debug
