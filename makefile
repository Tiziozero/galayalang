all: galac
	./galac main.gala -o:prog
	./prog
debug: galac
	./galac main.gala -o:prog -debug
	./prog
tour: galac tour.gala
	./galac tour.gala -o:tour -debug
	./tour
test: galac tests/test_language.gala
	./galac tests/test_language.gala -o:test
	./test

galac: *.odin
	odin build . -out:galac -debug
