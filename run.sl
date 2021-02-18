#!/bin/bash -e
#SBATCH --job-name=gps       # job name (shows up in queue)
#SBATCH --time=00-01:00:00   # Walltime (DD-HH:MM:SS)
#SBATCH --mem=10000          # total memory in MB
#SBATCH --cpus-per-task=1    # 1 CPUs

# load R
module load R

# run makefile
Rscript make.R
