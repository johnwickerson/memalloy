#!/bin/sh

GV=graphviz-2.40.1

wget http://www.graphviz.org/pub/graphviz/stable/SOURCES/$GV.tar.gz
tar -xzvf $GV.tar.gz
cd $GV
./configure \
    --disable-debug \
    --disable-dependency-tracking \
    --without-qt \
    --with-quartz
sudo make install
