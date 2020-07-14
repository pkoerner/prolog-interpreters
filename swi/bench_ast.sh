rm -f output_ast
touch output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_ast.pl -g "run(fib),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_ast.pl -g "run(fib_maxint),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_ast.pl -g "run(prime_tester),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_ast.pl -g "run(generated),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_ast.pl -g "run(generated2),halt." >> output_ast
done

echo "###########################################" >> output_ast

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_ast.pl -g "run(generated3),halt." >> output_ast
done

echo "###########################################" >> output_ast

