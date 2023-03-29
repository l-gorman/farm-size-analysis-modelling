#!/bin/bash

#SBATCH --job-name=farm-size-modelling
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=4-00:00:00
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

while getopts s:i:w:o:d:c: flag
do
  case "$flag" in 
    s) script=${OPTARG};;
    i) iterations=${OPTARG};;
    w) warmup=${OPTARG};;
    o) out_directory=${OPTARG};;
    d) data_directory=${OPTARG};;
    c) cores=${OPTARG};;
  esac
done

if [ -z "$script" ]
then
  echo "Need to include script path"; 
  exit;
else
  script="./src/modelling/${script}"
fi


if [ -z "$out_directory" ]
then
  out_directory="/user/work/lg14410/farm-size-analysis-modelling/outputs/"
else
  out_directory="/user/work/lg14410/farm-size-analysis-modelling/outputs/${out_directory}"
fi

if [ -z "$data_directory" ]
then
  data_directory="/user/work/lg14410/farm-size-analysis-modelling/data/"
else
  data_directory="/user/work/lg14410/farm-size-analysis-modelling/data/${data_directory}"
fi

if [ -z "$iterations" ]
then
  iterations=2000
else
  iterations=$iterations
fi

if [ -z "$warmup" ]
then
  warmup=1000
else
  warmup=$warmup
fi

if [ -z "$cores" ]
then
  cores=4
else
  cores=$cores
fi


echo "iterations: $iterations"
echo "warmup: $warmup"

echo "out_directory: $out_directory"
echo "data_directory: $data_directory"
echo "cores: $cores"



Rscript $script -i $iterations -w $warmup -d $data_directory -o $out_directory -c $cores

unset iterations
unset out_directory
unset data_directory
unset cores
unset script
