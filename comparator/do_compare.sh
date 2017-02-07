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

# process solution
gen/gen -Tdot -o exec.dot xml/test.xml

# visualise solution
dot -Tpng -o exec.png exec.dot && rm exec.dot
if [[ $OS = "x86-mac" ]]
then
    open exec.png
fi
