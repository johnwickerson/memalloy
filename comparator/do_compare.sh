ALS_FILE=$1
CMD=0
cd ..

export SOLVER=glucose

echo "Alloy started at `date -u`."

# prepare output directory
STAMP=`date +%y%m%d-%H%M%S`
XMLDIR=xml/$STAMP
mkdir -p $XMLDIR
cd xml; rm -f _latest; ln -s $STAMP _latest; cd ..

# run alloy
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
cd dot; rm -f _latest; ln -s $STAMP _latest; cd ..
cd $XMLDIR
for file in test_*.xml; do
    ../../gen/gen -Tdot -o ../../$DOTDIR/${file%.xml}.dot $file
done
cd ../..

# convert .dot to .png
echo "Converting DOT file(s) to PNG file(s)."
PNGDIR=png/$STAMP
mkdir -p $PNGDIR
cd png; rm -f _latest; ln -s $STAMP _latest; cd ..
cd $DOTDIR
for file in test_*.dot; do
    dot -Tpng -o ../../$PNGDIR/${file%.dot}.png $file
done
cd ../..

# visualise .png
if [ "$OS" = "x86-mac" ]
then
    open png/_latest
else
    echo "Solution(s) are in png/_latest."
fi
