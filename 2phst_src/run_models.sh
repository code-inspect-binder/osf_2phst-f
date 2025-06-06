#!/bin/bash
#SBATCH --mem-per-cpu 500
#SBATCH --array=1-531
#SBATCH --mail-type=END
#SBATCH --mail-user=jcf26@duke.edu

Rscript run_one_model.R "$1" "$2"
