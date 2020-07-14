mkdir -p tmp/
rm -f output_bc
touch output_bc

swipl -l bench_bc.pl -g "prepare(fib),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_bc.pl -g "run_prepared(fib),halt." >> output_bc
done

echo "########################################################" >> output_bc

swipl -l bench_bc.pl -g "prepare(fib_maxint),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_bc.pl -g "run_prepared(fib_maxint),halt." >> output_bc
done

echo "########################################################" >> output_bc

swipl -l bench_bc.pl -g "prepare(prime_tester),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_bc.pl -g "run_prepared(prime_tester),halt." >> output_bc
done

echo "########################################################" >> output_bc

swipl -l bench_bc.pl -g "prepare(generated),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_bc.pl -g "run_prepared(generated),halt." >> output_bc
done

echo "########################################################" >> output_bc

swipl -l bench_bc.pl -g "prepare(generated2),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_bc.pl -g "run_prepared(generated2),halt." >> output_bc
done

echo "########################################################" >> output_bc

swipl -l bench_bc.pl -g "prepare(generated3),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_bc.pl -g "run_prepared(generated3),halt." >> output_bc
done

echo "########################################################" >> output_bc
