
export PATH=$UFRC/Nek5000-David/bin:$PATH

genbox <<EOF 
$1.box
EOF

mv box.re2 $1.re2

genmap <<EOF
$1
.2
EOF

# change makenek
# 1 SOURCE_ROOT="$UFRC/Nek5000-David/" 
# 2 FC="mpif77  -mcmodel=large"
# 3 CC="mpicc  -mcmodel=large"
# 4 PPLIST="LPM"

./makenek $1
