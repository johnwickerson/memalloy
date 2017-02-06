ALS_FILE=$1
CMD=0
export SOLVER=glucose

echo "Alloy started at `date -u`."

# run alloy
cd ../alloystar
./runalloy_once.sh ../comparator/$ALS_FILE $CMD

echo "Alloy finished at `date -u`."

# move generated XML file(s) into their own directory
mkdir -p ../xml
mv test.xml ../xml/test.xml

# visualise solution
cd ../gen
./gen ../xml/test.xml > ../exec.dot
cd ..
dot -Tpng -o exec.png exec.dot && rm exec.dot
