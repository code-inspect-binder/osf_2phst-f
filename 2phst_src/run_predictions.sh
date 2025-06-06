#!/bin/bash
#SBATCH --mem-per-cpu 2G
#SBATCH --array=1-531
#SBATCH --mail-type=END
#SBATCH --mail-user=jcf26@duke.edu
#SBATCH --time=00:20:00

Rscript save_diffusion_results_test.R "$1"
