#!/bin/bash


Local=$(pwd)
echo $Local


for ang in {06..46..04}
do
#    mkdir AoA$ang
    let beta=$ang-5
    echo $ang

    cd AoA05
#    cp datain datain.bc init.input CFJ_block.input cfj_aoa5_sst5_original_601_151-8b.x init-1263.out main-1263-wmles.out  processing.out run.job ../AoA$ang

    cd $Local
    cd AoA$ang
#    rm 6*
    ./processing.out <<EOF >/dev/null
    1
    cfj_aoa5_sst5_original_601_151-8b.x
    2
    1
    $beta
    0.25
    0
EOF

#    bsub < run.job

#  next
    cd $Local
done
