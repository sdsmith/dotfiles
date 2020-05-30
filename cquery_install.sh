#!/bin/bash
# Setup cquery

if command -v cquery &>/dev/null; then
    echo "cquery installed, nothing to do"
    exit 0
fi

# Requires sudo
[ "$UID" -eq 0 ] || exec sudo bash "$0" "$@"

# Build
INSTALL_PREFIX=
if uname -a | grep -qE "Darwin" &> /dev/null ; then
    echo "macOS not supported yet"
    exit 1
else # running on linux
    INSTALL_PREFIX=/usr/local
fi

if ! command -v cmake &>/dev/null ; then
    echo "WARNING: please install cmake"
    exit 1
fi

pushd .
git clone --recursive https://github.com/cquery-project/cquery.git
cd cquery
git submodule update --init
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
cmake --build .
cmake --build . --target install
cd ..
rm -rf cquery
popd
