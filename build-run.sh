mkdir -p target
ghc --make src/main.hs src/Solver.hs -o target/main

./run.sh $1 $2 $3