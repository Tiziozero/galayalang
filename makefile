all: galac
	./galac main.gala -o:prog -l:libraylib.so.620
	LD_LIBRARY_PATH=. ./prog
debug: galac
	./galac main.gala -o:prog -debug
	LD_LIBRARY_PATH=. ./prog
tour: galac tour.gala
	./galac tour.gala -o:tour -debug
	.LD_LIBRARY_PATH=. /tour
test: galac tests/test_language.gala
	./galac tests/test_language.gala -o:test -debug
	LD_LIBRARY_PATH=. ./test

galac: *.odin
	odin build . -out:galac -debug
