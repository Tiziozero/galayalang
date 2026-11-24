all: galac
	./galac main.gala -o:prog
	./prog
debug: galac
	./galac main.gala -o:prog -debug
	./prog
tour: galac
	./galac tour.gala -o:tour -debug
	./tour

galac: *.odin
	odin build . -out:galac -debug
