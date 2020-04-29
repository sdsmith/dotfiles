#!/bin/zsh

pushd .

# Build
git clone https://github.com/google/benchmark.git
git clone https://github.com/google/googletest.git benchmark/googletest
cd benchmark
mkdir build && cd build
cmake ../
make

# Test install
cd build
make test

popd
