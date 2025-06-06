#!/bin/bash

export PATH=/opt/apps/slurm/R-3.2.3/bin:$PATH

for m in fit2d fit3d fit4d fit5d
do
  sbatch -J "$m" -o "${m}_%A_%a.out" -e "${m}_%A_%a.err" ./run_mse.sh "$m"
done
