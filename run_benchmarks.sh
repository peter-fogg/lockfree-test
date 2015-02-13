echo "Begin benchmarks for lockfree-test."
set -x
set -e

EXTRAARGS=$*

# Criterion regressions
REGRESSES="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters \
--regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters \
--regress=cpuTime:iters "

if [ "$MACHINECLASS" == "" ]; then
    export MACHINECLASS=`hostname -s`
fi

echo "On linux platforms, check CPU affinity:"
taskset -pc $$ || echo ok

echo "Also check load:"
sar 2 2 || echo ok

echo "And who"
who -a || echo ok

# Switch to the top of the repo:
cd `dirname $0`

echo "\nReturned to benchmarking script."

# CONVENTION: The working directory is passed as the first argument.
CHECKOUT=$1
shift || echo ok
if [ "$CHECKOUT" == "" ]; then
    CHECKOUT=`pwd`
fi

if [ "$JENKINS_GHC" == "" ]; then
    echo "JENKINS_GHC unset"
    export JENKINS_GHC=7.8.3
fi

echo "Running benchmarks remotely on server `hostname`"
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh" ]; then
    source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh
fi

which cabal
cabal --version
which ghc
ghc --version

executable=bench-lockfree-test

cabal sandbox init

echo "Installing benchmark program."

TAG=`date +'%s'`

which -a ghc-$JENKINS_GHC
cabal install -w ghc-$JENKINS_GHC --with-ghc-pkg=ghc-pkg-$JENKINS_GHC --enable-benchmarks $EXTRAARGS
cabal configure --enable-benchmarks
cabal build ${executable}

REPORT=report_${executable}

for i in 1 2 4 8 16 32; do
    CRITREPORT=$REPORT-N$i.crit
    time ./dist/build/$executable/$executable "AdaptiveBag/" "PureBag/" "ScalableBag/" --raw $CRITREPORT $REGRESSES +RTS -T -s -N$i -ls
done
