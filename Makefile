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
	dune exec ./runner/runner.exe

analyze:
	dune exec ./runner/analyzer.exe

full:
	dune exec ./runner/runner.exe
	dune exec ./runner/analyzer.exe
