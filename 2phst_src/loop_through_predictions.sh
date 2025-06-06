#!/bin/bash

for m in fit2d fit3d fit4d fit5d
do
  sbatch -J "$m" -o "${m}_%A_%a.out" -e "${m}_%A_%a.err" ./run_predictions.sh "$m"
done
