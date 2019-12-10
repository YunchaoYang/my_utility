#!/bin/bash

Local=$(pwd)
echo $Local

    amp='189.0'
    amp1='126.0'
    amp2=1.0009

for ang in {06..22..04}
do
    cd AoA$ang

    for cmcoef in {25..25..05}
    do 

    cd Cmu.$cmcoef

     cp init.input init.input0
     awk '{if($1=="6,1,1,")$3='$amp'; print $0}' init.input  >  init.input1  
     awk '{if($1=="8,1,1,")$2='$amp1'; print $0}' init.input1  >  init.input2  
     cp init.input2 init.input

#    bsub < run.job

    cd ../
    done


    cd $Local
done
