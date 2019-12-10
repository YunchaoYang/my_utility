IMG_WIDTH=100.0
IMG2_WIDTH=200
#RESULT=$(awk "BEGIN {printf \"%.2f\",${IMG_WIDTH}/${IMG_H}}")
echo "scale=2; 2/3" | bc

RESULT=$(echo "$IMG_WIDTH/$IMG2_WIDTH" | bc -l)
echo $RESULT