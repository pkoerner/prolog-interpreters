.PHONY: benchmarks

install:
	splfr --resource=driver --source=c_driven.pl driver.c stack.c --cflag="-Wall" --cflag="-O3"
benchmarks:
	sicstus -l benchmarker.pl --goal "benchs_to_files,halt."
bench: benchmarks
	sicstus -l benchmarker.pl --goal "run_bench,halt."
testall:
	sicstus -l benchmarker.pl --goal "plunit:run_tests,halt."
test:
	sicstus -l bc_interpreter.pl --goal "plunit:run_tests,halt."
c_interpreter: interpreter.c stack.c stack.h objspace.c objspace.h
	gcc -o c_interpreter interpreter.c stack.c objspace.c -Wall -g -O3 -DC_INTERPRETER
