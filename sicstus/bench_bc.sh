mkdir -p tmp/
rm -f output_bc
touch output_bc

sicstus -l bench_bc.pl --goal "prepare(fib),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_bc.pl --goal "run_prepared(fib),halt." >> output_bc
done

echo "########################################################" >> output_bc

sicstus -l bench_bc.pl --goal "prepare(fib_maxint),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_bc.pl --goal "run_prepared(fib_maxint),halt." >> output_bc
done

echo "########################################################" >> output_bc

sicstus -l bench_bc.pl --goal "prepare(prime_tester),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_bc.pl --goal "run_prepared(prime_tester),halt." >> output_bc
done

echo "########################################################" >> output_bc

sicstus -l bench_bc.pl --goal "prepare(generated),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_bc.pl --goal "run_prepared(generated),halt." >> output_bc
done

echo "########################################################" >> output_bc

sicstus -l bench_bc.pl --goal "prepare(generated2),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_bc.pl --goal "run_prepared(generated2),halt." >> output_bc
done

echo "########################################################" >> output_bc

sicstus -l bench_bc.pl --goal "prepare(generated3),halt." >> output_bc
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_bc.pl --goal "run_prepared(generated3),halt." >> output_bc
done

echo "########################################################" >> output_bc

