mkdir -p tmp/
rm -f output_c
touch output_c

sicstus -l bench_c.pl --goal "prepare(fib),halt." >> output_c
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_c.pl --goal "run_prepared(fib),halt." >> output_c
done

echo "#####################################" >> output_c

sicstus -l bench_c.pl --goal "prepare(fib_maxint),halt." >> output_c
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_c.pl --goal "run_prepared(fib_maxint),halt." >> output_c
done

echo "#####################################" >> output_c

sicstus -l bench_c.pl --goal "prepare(prime_tester),halt." >> output_c
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_c.pl --goal "run_prepared(prime_tester),halt." >> output_c
done

echo "#####################################" >> output_c

sicstus -l bench_c.pl --goal "prepare(generated),halt." >> output_c
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_c.pl --goal "run_prepared(generated),halt." >> output_c
done

echo "########################################################" >> output_c

sicstus -l bench_c.pl --goal "prepare(generated2),halt." >> output_c
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_c.pl --goal "run_prepared(generated2),halt." >> output_c
done

echo "########################################################" >> output_c

sicstus -l bench_c.pl --goal "prepare(generated3),halt." >> output_c
for i in 1 2 3 4 5 6 7 8 9 10
do
    sicstus -l bench_c.pl --goal "run_prepared(generated3),halt." >> output_c
done

echo "########################################################" >> output_c

