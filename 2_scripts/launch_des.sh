#!/bin/bash
#SBATCH --job-name=des
#SBATCH --output=/home/chenyangsu/colonoscopy-discrete-event-sim/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/chenyangsu/colonoscopy-discrete-event-sim/3_intermediate/slurm_output/%j.err
#SBATCH --time=1:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen-yang.su@mail.mcgill.ca  



cd $HOME/colonoscopy-discrete-event-sim

Rscript ./2_scripts/des.R
