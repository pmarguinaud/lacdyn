#!/bin/bash

set -xe

cd cpu
make ARCH=CPU -f ../makefile -j
cd ..

cd gpu
make ARCH=GPU -f ../makefile -j
cd ..

#cd kpu
#make ARCH=KPU -f ../makefile -j
#cd ..

\rm cpu/out.txt
\rm gpu/out.txt
./cpu/wrap_lacdyn.x --case t0031  > cpu/out.txt 2>&1
./gpu/wrap_lacdyn.x --case t0031  > gpu/out.txt 2>&1

vim -d */out.txt
