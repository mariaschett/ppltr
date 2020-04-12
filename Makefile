build :
	dune build @install

run : build
	dune exec ./main.exe

test : build
	dune runtest

test_% : build
	dune exec ./test/$@.exe

clean :
	dune clean

.PHONY : build run test test_% utop clean
