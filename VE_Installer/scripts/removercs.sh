#!/bin/sh -f
cp $1 $1.old
cat $1.old | sed '/\$RCSfile:/d' > $1
rm $1.old