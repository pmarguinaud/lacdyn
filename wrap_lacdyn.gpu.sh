#!/bin/bash
#SBATCH --nodes=1
#SBATCH --account=hun@gpu
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --gres=gpu:2

module load nvidia-compilers/21.5

set -x
set -e

cd /gpfswork/rech/jau/ufh62jk/lacdyn/openacc-kernels

hostname

./scripts/compile.pl --update --compile --bin wrap_lacdyn.x --arch gpu > compile.gpu.eo 2>&1

 ./compile.gpu/wrap_lacdyn.x --case t1198 --heapsize 100 --single-block --fix-arrays --diff
#nvprof --print-gpu-trace ./compile.gpu/wrap_lacdyn.x --case t1198 --heapsize 100 --single-block --fix-arrays 

# nsys profile -f true -o lacdyn.qdrep ./wrap_lacdyn.x --case t1198 --heapsize 100 # --diff --diff-block-list 1 
# nvprof  --print-gpu-trace ./wrap_lacdyn.x --case t1198 --heapsize 100 --diff 