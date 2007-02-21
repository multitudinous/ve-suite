#!/bin/sh -f
if [ -n "`grep "auto-copyright.pl BEGIN" $1`" ] ; then
     echo "$1 - Change header and activate props"
     mv $1 $1.old
     sed -f $VE_SUITE_HOME/VE_Installer/cmdfileKeywords "$1.old" > "$1"
     rm -f $1.old
     svn propset svn:keywords "Date Author Revision Id" $1
else
     echo "$1 - Does not contain a header:"
fi
