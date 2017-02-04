MEMALLOY_HOME=`pwd`

ALS_FILE=$MEMALLOY_HOME/tests/Q2_c11_sra_simp.als
CMD=1
export SOLVER=glucose

#EVENT_COUNT=$1

# modify the .als file to search for solutions using
# the given number of events
#sed -i -e "s/exactly [0-9]\+/exactly $EVENT_COUNT/g" $ALS_FILE

echo "Alloy started at `date -u`."

cd alloystar
./runalloy_once.sh $ALS_FILE $CMD

echo "Alloy finished at `date -u`."

# move generated XML file(s) into their own directory
mkdir -p $MEMALLOY_HOME/xml
mv test.xml $MEMALLOY_HOME/xml/test.xml

cd ../gen
./gen ../xml/test.xml > ../exec.dot
cd ..
dot -Tpng -o exec.png exec.dot
#open exec.png
