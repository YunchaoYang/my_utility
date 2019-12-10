

for f in flow*.plt
do
  ./preplot $f Tran$f
 # do something on $f
  echo $f
done
rm flow*
echo "preplot finished"