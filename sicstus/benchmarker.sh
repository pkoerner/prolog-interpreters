sicstus -l benchmarker.pl --goal "benchs_to_files,halt."
rm -f output_ast
rm -f output_bc
rm -f output_c
rm -f output_facts
rm -f output_rt

for i in 1 2 3 4 5 6 7 8 9 10
do
sicstus -l bench_ast.pl --goal "run(prime_tester),halt." >> output_ast
sicstus -l bench_bc     --goal "run(prime_tester),halt." >> output_bc
sicstus -l bench_c      --goal "run(prime_tester),halt." >> output_c
sicstus -l bench_facts  --goal "run(prime_tester),halt." >> output_facts
sicstus -l bench_rt     --goal "run(prime_tester),halt." >> output_rt
echo "############################" >> output_ast
echo "############################" >> output_bc
echo "############################" >> output_c
echo "############################" >> output_facts
echo "############################" >> output_rt
done

for i in 1 2 3 4 5 6 7 8 9 10
do
sicstus -l bench_ast.pl --goal "run(fib),halt." >> output_ast
sicstus -l bench_bc     --goal "run(fib),halt." >> output_bc
sicstus -l bench_c      --goal "run(fib),halt." >> output_c
sicstus -l bench_facts  --goal "run(fib),halt." >> output_facts
sicstus -l bench_rt     --goal "run(fib),halt." >> output_rt
echo "############################" >> output_ast
echo "############################" >> output_bc
echo "############################" >> output_c
echo "############################" >> output_facts
echo "############################" >> output_rt
done

for i in 1 2 3 4 5 6 7 8 9 10
do
sicstus -l bench_ast.pl --goal "run(generated),halt." >> output_ast
sicstus -l bench_bc     --goal "run(generated),halt." >> output_bc
sicstus -l bench_c      --goal "run(generated),halt." >> output_c
sicstus -l bench_facts  --goal "run(generated),halt." >> output_facts
sicstus -l bench_rt     --goal "run(generated),halt." >> output_rt
echo "############################" >> output_ast
echo "############################" >> output_bc
echo "############################" >> output_c
echo "############################" >> output_facts
echo "############################" >> output_rt
done
