#!/bin/bash
#PBS -N deconvSplit
#PBS -l nodes=1
#PBS -l cput=00:30:00

cd $PBS_O_WORKDIR
source setupLDPath.sh
echo $LD_LIBRARY_PATH
echo mpirun -machinefile $PBS_NODEFILE -np 1 /sad/kgpm/sdat/radiancesDA_serial/bin/deconvolverObsRes.exe $orbitFile $platform $posting $outputName $splitNum $nsplit $bhalf
     mpirun -machinefile $PBS_NODEFILE -np 1 /sad/kgpm/sdat/radiancesDA_serial/bin/deconvolverObsRes.exe $orbitFile $platform $posting $outputName $splitNum $nsplit $bhalf
