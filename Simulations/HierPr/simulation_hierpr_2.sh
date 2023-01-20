#!/bin/bash
#SBATCH -o /dss/dsshome1/lxc03/di73set/outfiles/myjob.%j.%N.out
#SBATCH -D /dss/dsshome1/lxc03/di73set
#SBATCH -J simulation_hierpr_2
#SBATCH --get-user-env 
#SBATCH --clusters=cm2
#SBATCH --partition=cm2_std
#SBATCH --qos=cm2_std
#SBATCH --nodes=5
#SBATCH --tasks-per-node=20
#SBATCH --mail-type=end
#SBATCH --mail-user=hornung@ibe.med.uni-muenchen.de
#SBATCH --time=5:00:00

module load slurm_setup
module load r
module load openmpi

mpirun /dss/dsshome1/lxc03/di73set/RMPISNOWscript/RMPISNOW < ./PPerfEstComplex/simulations/hierpr/simulation_2.R