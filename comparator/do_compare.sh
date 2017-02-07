ALS_FILE=$1
CMD=0
export SOLVER=glucose

echo "Alloy started at `date -u`."
STAMP=`date +%y%m%d-%H%M%S`

# run alloy
cd ..
XMLDIR=xml/$STAMP
mkdir -p $XMLDIR
ln -s -f $XMLDIR xml/_latest

cd alloystar
./runalloy_once.sh ../comparator/$ALS_FILE $CMD ../$XMLDIR
ALLOY_EXIT_STATUS=$?
cd ..

echo "Alloy finished at `date -u` with code $ALLOY_EXIT_STATUS."

if [ "$ALLOY_EXIT_STATUS" = "0" ]
then
    continue
else
    exit 0
fi

# convert solution to .dot
echo "Converting XML file(s) to DOT file(s)."
DOTDIR=dot/$STAMP
mkdir -p $DOTDIR
ln -s -f $DOTDIR dot/_latest
gen/gen -Tdot -o $DOTDIR/exec.dot $XMLDIR/test_0.xml

# convert .dot to .png
echo "Converting DOT file(s) to PNG file(s)."
PNGDIR=png/$STAMP
mkdir -p $PNGDIR
ln -s -f $PNGDIR png/_latest
dot -Tpng -o $PNGDIR/exec.png $DOTDIR/exec.dot

# visualise .png
if [ "$OS" = "x86-mac" ]
then
    open $PNGDIR/_latest
else
    echo "Solution(s) are in $PNGDIR/_latest."
fi
