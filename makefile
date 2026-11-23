all: galac
	./galac main.gala -o:prog
	./prog
debug: galac
	./galac main.gala -o:prog -debug
	./prog

galac: *.odin
	odin build . -out:galac -debug
