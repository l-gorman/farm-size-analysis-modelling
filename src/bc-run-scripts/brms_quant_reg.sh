#!/bin/bash

#SBATCH --job-name=farm-size-modelling
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-01:00:00
#SBATCH --mem=36G
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

while getopts s:i:o:d:c: flag
do
  case "$flag" in 
    i) iterations=${OPTARG};;
    o) out_directory=${OPTARG};;
    d) data_directory=${OPTARG};;
    c) cores=${OPTARG};;

  esac
done

if [ -z "$iterations" ]
then
  $iterations=4000
fi

if [ -z "$my_var" ]
then
  out_directory="random directory"
fi

echo "iterations: $iterations"
echo "out_directory: $out_directory"




Rscript "src/modelling/brms_quant_reg.R" -i $iterations -d "brms-27-01-2023" -b "/user/work/lg14410/farm-size-modelling/" -c 4

