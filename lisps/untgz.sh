#!/bin/sh

# Time-stamp: <05/18/2009 09:52:26 星期一 by ahei>

bin=`dirname "$0"`
cd "$bin"

for i in `ls *gz`; do tar xzf $i; done
for i in `ls *bz2`; do tar jxf $i; done

cd - > /dev/null
