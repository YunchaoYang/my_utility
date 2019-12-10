#!/bin/bash

Local=$(pwd)
echo $Local

for ang in {10..46..04}
do
    cd AoA$ang
# save
    mkdir Re3.9
    cp datain init.input cdlt.his forces.his jet.his Re3.9
    mv rstore* Re3.9
    rm rstart*

#start new
    cp datain datain00

    sed -i "s/reynolds.*/reynolds    = 3.03d6 , /g" datain
    sed -i "s/poutlet.*/poutlet     = 150.03 ,  /g" datain
    sed -i "s/ptotal.*/ptotal      = 150.53 ,   /g" datain
    sed -i "s/ttotal.*/ttotal      = 1.0009522, /g" datain

    sed -i "s/choice.*/choice = 'n', /g" datain

    sed -i "s/inj_ampli.*/inj_ampli = 100, /g" datain
    sed -i "s/suc_ampli.*/suc_ampli = 100, /g" datain
    sed -i "s/index.*/index = 20000, /g" datain

#init.input
    sed -i "s/6,1,1,.*/6,1,1, 155, 156, 1.00079806, /g" init.input
    sed -i "s/8,1,1,.*/8,1,1, 145, 146, 1.00079806, /g" init.input
# submit
    bsub < run.job

################################
    for cmcoef in {10..10..05}
    do 

    cd Cmu.$cmcoef

# save
    cp ../bin* .
    mkdir Re3.9
    cp datain init.input cdlt.his forces.his jet.his Re3.9
    mv rstore* Re3.9
    rm rstart*

    cp datain datain00

    sed -i "s/reynolds.*/reynolds    = 3.03d6 , /g" datain
    sed -i "s/poutlet.*/poutlet     = 150.03 ,  /g" datain
    sed -i "s/ptotal.*/ptotal      = 150.53 ,   /g" datain
    sed -i "s/ttotal.*/ttotal      = 1.0009522, /g" datain

    sed -i "s/choice.*/choice = 'n', /g" datain

    sed -i "s/inj_ampli.*/inj_ampli = 100, /g" datain
    sed -i "s/suc_ampli.*/suc_ampli = 100, /g" datain
    sed -i "s/index.*/index = 20000, /g" datain

    sed -i "s/6,1,1,.*/6,1,1, 164, 165, 1.00079806, /g" init.input

    sed -i "s/8,1,1,.*/8,1,1, 134, 135, 1.00079806, /g" init.input

    bsub < run.job

    cd ../
    done

################################
    for cmcoef in {15..15..05}
    do 

    cd Cmu.$cmcoef
# save
    cp ../bin* .
    mkdir Re3.9
    cp datain init.input cdlt.his forces.his jet.his Re3.9
    mv rstore* Re3.9
    rm rstart*

#
    cp datain datain00

    sed -i "s/reynolds.*/reynolds    = 3.03d6 , /g" datain
    sed -i "s/poutlet.*/poutlet     = 150.03 ,  /g" datain
    sed -i "s/ptotal.*/ptotal      = 150.53 ,   /g" datain
    sed -i "s/ttotal.*/ttotal      = 1.0009522, /g" datain

    sed -i "s/choice.*/choice = 'n', /g" datain

    sed -i "s/inj_ampli.*/inj_ampli = 100, /g" datain
    sed -i "s/suc_ampli.*/suc_ampli = 100, /g" datain
    sed -i "s/index.*/index = 20000, /g" datain

    sed -i "s/6,1,1,.*/6,1,1, 172, 173, 1.00079806, /g" init.input

    sed -i "s/8,1,1,.*/8,1,1, 125, 125, 1.00079806, /g" init.input

    bsub < run.job

    cd ../
    done

################################
    for cmcoef in {20..20..05}
    do 

    cd Cmu.$cmcoef

# save
    cp ../bin* .
    mkdir Re3.9
    cp datain init.input cdlt.his forces.his jet.his Re3.9
    mv rstore* Re3.9
    rm rstart*
    
    cp datain datain00

    sed -i "s/reynolds.*/reynolds    = 3.03d6 , /g" datain
    sed -i "s/poutlet.*/poutlet     = 150.03 ,  /g" datain
    sed -i "s/ptotal.*/ptotal      = 150.53 ,   /g" datain
    sed -i "s/ttotal.*/ttotal      = 1.0009522, /g" datain

    sed -i "s/choice.*/choice = 'n', /g" datain

    sed -i "s/inj_ampli.*/inj_ampli = 100, /g" datain
    sed -i "s/suc_ampli.*/suc_ampli = 100, /g" datain
    sed -i "s/index.*/index = 20000, /g" datain

    sed -i "s/6,1,1,.*/6,1,1, 180, 182, 1.00079806, /g" init.input

    sed -i "s/8,1,1,.*/8,1,1, 117, 120, 1.00079806, /g" init.input

    bsub < run.job

    cd ../
    done

################################
    for cmcoef in {25..25..05}
    do 

    cd Cmu.$cmcoef
# save
    cp ../bin* .
    mkdir Re3.9
    cp datain init.input cdlt.his forces.his jet.his Re3.9
    mv rstore* Re3.9
    rm rstart*

    cp datain datain00

    sed -i "s/reynolds.*/reynolds    = 3.03d6 , /g" datain
    sed -i "s/poutlet.*/poutlet     = 150.03 ,  /g" datain
    sed -i "s/ptotal.*/ptotal      = 150.53 ,   /g" datain
    sed -i "s/ttotal.*/ttotal      = 1.0009522, /g" datain

    sed -i "s/choice.*/choice = 'n', /g" datain

    sed -i "s/inj_ampli.*/inj_ampli = 100, /g" datain
    sed -i "s/suc_ampli.*/suc_ampli = 100, /g" datain
    sed -i "s/index.*/index = 20000, /g" datain

    sed -i "s/6,1,1,.*/6,1,1, 190, 190, 1.00079806, /g" init.input

    sed -i "s/8,1,1,.*/8,1,1, 115, 115, 1.00079806, /g" init.input

    bsub < run.job

    cd ../
    done


    cd $Local
done
