#!/bin/bash

# Check if a filename is provided as the first argument
if [ $# -ne 1 ]; then
    echo "Error: Please provide a filename as the first argument."
    exit 1
fi

# Assign the filename to a variable
filename=$1

# Run the diablo compiler
dune exec -- delta $filename

# Run the LLVM compiler
clang-19 -target x86_64-pc-linux-gnu llvm_bin/output.ll -o llvm_bin/output
