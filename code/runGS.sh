#!/bin/bash
#SBATCH -A lmshannon
#SBATCH --nodes=1
#SBATCH --ntasks=16
#SBATCH --mem=64gb
#SBATCH --time=48:00:00
#SBATCH --mail-type=All
#SBATCH --mail-user=yusuf223@umn.edu
#SBATCH -p msismall
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -J GS_spectra_multistage



datadir=/home/lmshannon/yusuf223/gradProject/GS_multispectra_analysis

cd $datadir
#module load vcflib
#module load R/4.3.0-openblas
module load R/4.3.0-openblas-rocky8-fix

Rscript ./code/runGSspec_bnI.R
#Rscript ./code/runGSspec_all.R
#Rscript ./code/runSelvarGS_all.R
