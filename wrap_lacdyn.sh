#!/bin/bash
#SBATCH --nodes=1
#SBATCH --account=hun@gpu
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --gres=gpu:2

module load nvidia-compilers/21.5

set -x

cd /gpfswork/rech/jau/ufh62jk/lacdyn/openacc-notmanaged

hostname

make wrap_lacdyn.x

 ./wrap_lacdyn.x --case t0031 --diff --heapsize 100
# nsys profile -f true -o lacdyn.qdrep ./wrap_lacdyn.x --case t1198 --heapsize 100 # --diff --diff-block-list 1 
#./wrap_lacdyn.x --case t1198 --heapsize 100 # --diff --diff-block-list 1 
