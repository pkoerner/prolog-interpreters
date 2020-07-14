.PHONY: benchmarks

install:
	swipl-ld -shared -o driver driver.c stack.c -cc-options,-Wall -cc-options,-g -cc-options,-O3
benchmarks:
	swipl -l benchmarker.pl -g "benchs_to_files,halt."
bench: 
	swipl -O -l benchmarker.pl -g "run_bench,halt."
testall:
	swipl -l benchmarker.pl -g "plunit:run_tests,halt."
test:
	swipl -l bc_interpreter.pl --goal "plunit:run_tests,halt."
c_interpreter: interpreter.c stack.c stack.h objspace.c objspace.h
	gcc -o c_interpreter interpreter.c stack.c objspace.c -Wall -g -O3
