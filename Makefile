all:  runner analyzer
.PHONY: all


.PHONY: runner
runner:
	dune build ./runner/runner.exe

.PHONY: analyzer
analyzer:
	dune build ./analyzer/analyzer.exe


clean:
	dune clean

run:
	cset shield --exec dune -- exec ./runner/runner.exe

analyze:
	dune exec ./analyzer/analyzer.exe -- -output-dir pr10337 -log pr10337/longer_complex.log

full:
	dune exec ./runner/runner.exe
	dune exec ./analyzer/analyzer.exe
