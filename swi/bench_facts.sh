mkdir -p tmp/
rm -f output_facts
touch output_facts

swipl -l bench_facts.pl -g "prepare(fib),halt." >> output_facts
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_facts.pl -g "run_prepared(fib),halt." >> output_facts
done

echo "############################################" >> output_facts

swipl -l bench_facts.pl -g "prepare(fib_maxint),halt." >> output_facts
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_facts.pl -g "run_prepared(fib_maxint),halt." >> output_facts
done

echo "############################################" >> output_facts

swipl -l bench_facts.pl -g "prepare(prime_tester),halt." >> output_facts
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_facts.pl -g "run_prepared(prime_tester),halt." >> output_facts
done

echo "############################################" >> output_facts

swipl -l bench_facts.pl -g "prepare(generated),halt." >> output_facts
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_facts.pl -g "run_prepared(generated),halt." >> output_facts
done

echo "############################################" >> output_facts

swipl -l bench_facts.pl -g "prepare(generated2),halt." >> output_facts
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_facts.pl -g "run_prepared(generated2),halt." >> output_facts
done

echo "############################################" >> output_facts

swipl -l bench_facts.pl -g "prepare(generated3),halt." >> output_facts
for i in 1 2 3 4 5 6 7 8 9 10
do
    swipl -l bench_facts.pl -g "run_prepared(generated3),halt." >> output_facts
done

echo "############################################" >> output_facts
