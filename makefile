all: galac
	./galac main.gala -o:prog
	./prog
debug: galac
	./galac main.gala -o:prog -debug
	./prog
tour: galac
	./galac tour.gala -o:prog -debug
	./tour

galac: *.odin
	odin build . -out:galac -debug
