mkdir -p tmp/
rm -f output_rt
touch output_rt

swipl -l bench_rt.pl -g "prepare(fib),halt." >> output_rt
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_rt.pl -g "run_prepared(fib),halt." >> output_rt
done

echo "#######################################" >> output_rt

swipl -l bench_rt.pl -g "prepare(fib_maxint),halt." >> output_rt
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_rt.pl -g "run_prepared(fib_maxint),halt." >> output_rt
done

echo "#######################################" >> output_rt

swipl -l bench_rt.pl -g "prepare(prime_tester),halt." >> output_rt
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_rt.pl -g "run_prepared(prime_tester),halt." >> output_rt
done

echo "#######################################" >> output_rt

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_rt.pl -g "run(generated),halt." >> output_rt
done

echo "#######################################" >> output_rt

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_rt.pl -g "run(generated2),halt." >> output_rt
done

echo "#######################################" >> output_rt

for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_rt.pl -g "run(generated3),halt." >> output_rt
done

echo "#######################################" >> output_rt
