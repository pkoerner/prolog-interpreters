rm -f output_ast
rm -f output_bc
rm -f output_c
rm -f output_facts
rm -f output_rt

for i in 1 2 3 4 5 6 7 8 9 10
do
swipl -l bench_ast.pl -g "run(prime_tester),halt." >> output_ast
swipl -l bench_bc     -g "run(prime_tester),halt." >> output_bc
swipl -l bench_c      -g "run(prime_tester),halt." >> output_c
swipl -l bench_facts  -g "run(prime_tester),halt." >> output_facts
swipl -l bench_rt     -g "run(prime_tester),halt." >> output_rt
echo "############################" >> output_ast
echo "############################" >> output_bc
echo "############################" >> output_c
echo "############################" >> output_facts
echo "############################" >> output_rt
done

for i in 1 2 3 4 5 6 7 8 9 10
do
swipl -l bench_ast.pl -g "run(fib),halt." >> output_ast
swipl -l bench_bc     -g "run(fib),halt." >> output_bc
swipl -l bench_c      -g "run(fib),halt." >> output_c
swipl -l bench_facts  -g "run(fib),halt." >> output_facts
swipl -l bench_rt     -g "run(fib),halt." >> output_rt
echo "############################" >> output_ast
echo "############################" >> output_bc
echo "############################" >> output_c
echo "############################" >> output_facts
echo "############################" >> output_rt
done

for i in 1 2 3 4 5 6 7 8 9 10
do
swipl -l bench_ast.pl -g "run(generated),halt." >> output_ast
swipl -l bench_bc     -g "run(generated),halt." >> output_bc
swipl -l bench_c      -g "run(generated),halt." >> output_c
swipl -l bench_facts  -g "run(generated),halt." >> output_facts
swipl -l bench_rt     -g "run(generated),halt." >> output_rt
echo "############################" >> output_ast
echo "############################" >> output_bc
echo "############################" >> output_c
echo "############################" >> output_facts
echo "############################" >> output_rt
done
