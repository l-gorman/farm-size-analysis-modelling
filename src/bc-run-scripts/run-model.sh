#!/bin/bash

#SBATCH --mail-user=[lg14410@bristol.ac.uk]
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --job-name=distributional-analysis
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=25
#SBATCH --time=0-00:00:30
#SBATCH --mem=36G


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

# d=$(date +%Y-%m-%d)

export OMP_NUM_THREADS=5


Rscript 02-exploratory-analysis.R -n 1000 -i 2000 -d "kfold_future_test" -b "/user/work/lg14410/farm-size-modelling/" -c 25

#for SAMPLE in  10000 20000  "MAX"

# for SAMPLE in "MAX"
# do
#   for ITER in  10000 20000
#   # for ITER in  2000
#   do
#     Rscript 02-exploratory-analysis.R -n $SAMPLE -i $ITER -d $d -b "/user/work/lg14410/farm-size-modelling/"
#   done
# done
