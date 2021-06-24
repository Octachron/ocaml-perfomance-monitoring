all: experience
.PHONY: all


experience:
	dune build experience.exe

clean:
	dune clean

run: all
	dune exec ./experience.exe
