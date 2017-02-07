ALS_FILE=$1
CMD=0
export SOLVER=glucose

echo "Alloy started at `date -u`."

# run alloy
cd ../alloystar
./runalloy_once.sh ../comparator/$ALS_FILE $CMD
cd ..

echo "Alloy finished at `date -u`."

# move generated XML file(s) into their own directory
mkdir -p xml
mv alloystar/test.xml xml/test.xml

# visualise solution
gen/gen -o exec.dot xml/test.xml
dot -Tpng -o exec.png exec.dot && rm exec.dot
