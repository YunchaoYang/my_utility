#!/bin/bash


Local=$(pwd)
echo $Local


for ang in {30..46..04}
do
    cd AoA$ang
    cp ../AoA05/datain .

    for cmcoef in {10..25..05}
    do 

    mkdir Cmu.$cmcoef
    
    cp datain datain.bc init.input CFJ_block.input bin* init-1263.out main-1263-wmles.out  processing.out run.job Cmu.$cmcoef

    cd Cmu.$cmcoef
    cmu=`echo - | awk '{print $cmcoef*0.01}'`
    cmu=$(echo "$cmcoef/100" | bc -l)
    echo $cmu
    cp datain datain1
     awk '{if($1=="cmu_target")$3='$cmu'; print $0}' datain  >  datain0  
     cp datain0 datain
    bsub < run.job

    cd ../
    done


    cd $Local
done
