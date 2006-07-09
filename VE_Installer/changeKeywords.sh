#!/bin/sh -f
cp $1 $1.old
sed -f $VE_SUITE_HOME/VE_Installer/cmdfileKeywords $1.old > $1
rm $1.old