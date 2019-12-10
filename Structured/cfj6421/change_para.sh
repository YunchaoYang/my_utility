#!/bin/bash


Local=$(pwd)
echo $Local
    amp='500'
    number='40000'


for ang in {02..46..04}
do
    cd AoA$ang
     cp datain datain1
     awk '{if($1=="inj_ampli")$3='$amp'; print $0}' datain  >  datain0  
     awk '{if($1=="suc_ampli")$3='$amp'; print $0}' datain0  >  datain2   
     awk '{if($1=="index")$3='$number'; print $0}' datain2  >  datain3   
     cp datain3 datain

    for cmcoef in {10..25..05}
    do 

    cd Cmu.$cmcoef
#    cmu=`echo - | awk '{print $cmcoef*0.01}'`
#    cmu=$(echo "$cmcoef/100" | bc -l)
#    echo $cmu
     cp datain datain1
     awk '{if($1=="inj_ampli")$3='$amp'; print $0}' datain  >  datain0  
     awk '{if($1=="suc_ampli")$3='$amp'; print $0}' datain0  >  datain2   
     awk '{if($1=="index")$3='$number'; print $0}' datain2  >  datain3   
     cp datain3 datain

#    bsub < run.job

    cd ../
    done


    cd $Local
done
