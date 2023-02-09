#!/bin/bash

#SBATCH --job-name=c_mu_q_brms_anova
#SBATCH -o ./Report/brms_comparison.%a.out
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=28
#SBATCH --time=1-00:00:00
#SBATCH --mem=60G
#SBATCH --account=sscm012844

cd "${SLURM_SUBMIT_DIR}"

echo "Running on host $(hostname) \n"
echo "Time is $(date) \n"
echo "Directory is $(pwd) \n"
echo "Slurm job ID is ${SLURM_JOBID} \n"
echo "This jobs runs on the following machines: \n"
echo "${SLURM_JOB_NODELIST}"

echo "Keep track of job by entering sacct -j ${SLURM_JOBID}  \n"
echo "Cancel your job by entering scancel ${SLURM_JOBID}  \n"
echo "More details on submitting jobs here https://www.acrc.bris.ac.uk/protected/hpc-docs/job_types/ \n"

module add languages/r/4.1.0

while getopts i:w:o:d:c: flag
do
  case "$flag" in 
    c) cores=${OPTARG};;
    o) out_directory=${OPTARG};;
    d) data_directory=${OPTARG};;
  esac
done



if [ -z "$out_directory" ]
then
  out_directory="/user/work/lg14410/farm-size-analysis-modelling/outputs/"
else
  out_directory="/user/work/lg14410/farm-size-analysis-modelling/outputs/${out_directory}"
fi

if [ -z "$data_directory" ]
then
  data_directory="/user/work/lg14410/farm-size-analysis-modelling/outputs/"
else
  data_directory="/user/work/lg14410/farm-size-analysis-modelling/outputs/${data_directory}"
fi



if [ -z "$cores" ]
then
  cores=4
else
  cores=$cores
fi




echo "out_directory: $out_directory"
echo "data_directory: $data_directory"
echo "cores: $cores"
echo "cores: $cores"



Rscript "src/evaluation/q_anov_eval.R" -d $data_directory -o $out_directory -c $cores -n "quantile_location_summary.csv"


unset iterations
unset out_directory
unset data_directory
