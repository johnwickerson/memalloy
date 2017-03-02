#!/bin/sh

#GVTYPE=stable
GVTYPE=development
#GV=graphviz-2.40.1
GV=graphviz-2.41.20170301.1954

wget http://www.graphviz.org/pub/graphviz/$GVTYPE/SOURCES/$GV.tar.gz
tar -xzvf $GV.tar.gz
cd $GV
./configure \
    --disable-debug \
    --disable-dependency-tracking \
    --without-qt \
    --with-quartz
sudo make install
