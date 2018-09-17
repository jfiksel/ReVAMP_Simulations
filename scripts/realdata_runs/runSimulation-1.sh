#!/bin/bash
#$ -l mem_free=9G
#$ -l h_vmem=9G
#$ -l h_rt=24:00:00
#$ -cwd
#$ -j y
#$ -R y
#$ -t 1-8000
Rscript 1-runSimulation.R $SGE_TASK_ID
