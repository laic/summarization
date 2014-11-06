#!/bin/bash

#$ -N ami-mlp
#$ -P inf_hcrc_cstr_inevent
#$ -pe memory-2G 2
#$ -cwd
#$ -o ./theano-tests
#$ -e ./theano-tests
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

#module load python
#module load cuda

export THEANO_FLAGS='device=cpu,pycuda.init=False,gcc.cxxflags=-m64 -L/usr/lib64/atlas/ -L/usr/lib64/,blas.ldflags=-lf77blas -latlas -lgfortran,on_unused_input=ignore'


# Run the program

PYSCRIPTS="../pyscripts/"

#dstem=$1
infile=$1
model=$2
n_in=$3
hlayers=$4
outfile=$5
testonly=$6

#datadir=/exports/home/clai/nn/data/$corpus/
#infile=$datadir/$dstem.pkl.gz

#hname=`echo $hlayers | tr " " "_"`
#outfile="${corpus}_mlp_${dstem}_${n_in}-$hname.pkl"

echo "apply-mlp.sh"
echo $infile
echo $outfile

python $PYSCRIPTS/mlp.py $infile $outfile $n_in $testonly $model $hlayers



