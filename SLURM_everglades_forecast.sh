#!/bin/bash
#SBATCH --job-name=wading_bird_forecast
#SBATCH --account=ewhite
#SBATCH --qos=ewhite
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32gb
#SBATCH --time=12:00:00
#SBATCH --output=/blue/ewhite/alexanderblochel/logs/forecast_%j.out
#SBATCH --error=/blue/ewhite/alexanderblochel/logs/forecast_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=alexanderblochel@ufl.edu

echo "INFO: [$(date "+%Y-%m-%d %H:%M:%S")] Starting forecast job on $(hostname)"
mkdir -p /blue/ewhite/alexanderblochel/logs

# --- Environment ---
source /etc/profile.d/modules.sh || exit 1
module load R/4.6
export R_LIBS_USER=/blue/ewhite/alexanderblochel/R/libs

cd /blue/ewhite/alexanderblochel/everglades-forecasting/ || exit 1

echo "INFO: [$(date "+%Y-%m-%d %H:%M:%S")] Running pipeline"

Rscript -e "
  PARALLEL <- FALSE
  options(mc.cores = 4)
  source('run_all_scales.R')
" || exit 1

echo "INFO: [$(date "+%Y-%m-%d %H:%M:%S")] Job completed successfully"