#!/bin/bash -e
#SBATCH --job-name=gps       # job name (shows up in queue)
#SBATCH --time=12:00:00      # Walltime (DD-HH:MM:SS)
#SBATCH --mem=60000          # total memory in MB
#SBATCH --cpus-per-task=4    # 4 CPUs

# load R
module load R

# run makefile
Rscript make.R