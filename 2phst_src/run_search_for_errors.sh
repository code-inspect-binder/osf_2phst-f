#!/bin/bash
#
#SBATCH --job-name=err_search
#SBATCH --output=error_search.out
#SBATCH --error=error_search.err
#SBATCH --mem=10
#SBATCH --time=01:00:00
#SBATCH -n=1
#SBATCH -c=1
#SBATCH --mail-user=jcf26@duke.edu
#SBATCH --mail-type=ALL

sbatch ~/social_space_diffusion/search_for_errors.sh
