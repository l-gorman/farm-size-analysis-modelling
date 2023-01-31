#!/bin/bash

while getopts s:i:o:d:c: flag
do
  case "$flag" in 
    s) script=${OPTARG};;
    i) iterations=${OPTARG};;
    o) out_directory=${OPTARG};;
    d) data_directory=${OPTARG};;
    c) cores=${OPTARG};;
  esac
done



if [ -z "$out_directory" ]
then
  out_directory="/user/work/lg14410/farm-size-modelling/"
else
  out_directory="/user/work/lg14410/farm-size-modelling/${out_directory}"
fi

if [ -z "$data_directory" ]
then
  data_directory="data/"
else
  data_directory="/user/work/lg14410/farm-size-modelling/${data_directory}"
fi

if [ -z "$iterations" ]
then
  iterations=4000
else
  iterations=$iterations
fi

if [ -z "$cores" ]
then
  cores=4
else
  cores=$cores
fi

if [ -z "$script" ]
then
  echo "Need to specify a script!" 1>&2
  exit 0
else
  script=$script
fi




echo "iterations: $iterations"
echo "out_directory: $out_directory"
echo "data_directory: $data_directory"
echo "cores: $cores"


unset iterations
unset out_directory
unset data_directory
unset cores


