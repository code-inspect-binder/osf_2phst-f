#!/bin/bash
declare -A models
declare -A times

models[fit2d]="net ~ euclidean(d = 2)"
models[fit3d]="net ~ euclidean(d = 3)"
models[fit4d]="net ~ euclidean(d = 4)"
models[fit5d]="net ~ euclidean(d = 5)"

times[fit2d]=0-02:30
times[fit3d]=0-05:00
times[fit4d]=0-10:00
times[fit5d]=1-00:00

for m in "${!models[@]}" 
do
  sbatch -J "$m" -o "${m}_%A_%a.out" -e "${m}_%A_%a.err" -t "${times[$m]}" ./run_models.sh "${models[$m]}" "$m"
done

