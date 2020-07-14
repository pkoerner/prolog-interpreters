rm -f output_ast
touch output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_ast.pl --goal "run(fib),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_ast.pl --goal "run(fib_maxint),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_ast.pl --goal "run(prime_tester),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_ast.pl --goal "run(generated),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_ast.pl --goal "run(generated2),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_ast.pl --goal "run(generated3),halt." >> output_ast
done

echo "###########################################" >> output_ast
