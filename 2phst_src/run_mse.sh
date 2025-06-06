#!/bin/bash
#SBATCH --mem-per-cpu 2G
#SBATCH --array=1-531
#SBATCH --mail-type=END
#SBATCH --mail-user=jcf26@duke.edu
#SBATCH --time=00:20:00

uname -n 1>&2
which Rscript 1>&2

Rscript calculate_one_mse.R "$1"
