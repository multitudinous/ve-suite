#!/bin/sh
#
# Author: Aaron Voisine <aaron@voisine.org>

CWD="`(cd \"\`dirname \\\"$0\\\"\`\"; echo $PWD)`"

ps -wx -ocommand | grep -e '[X]11' > /dev/null
if [ "$?" != "0" -a ! -f ~/.xinitrc ]; then
   echo "rm -f ~/.xinitrc" > ~/.xinitrc
   sed 's/xterm/# xterm/' /usr/X11R6/lib/X11/xinit/xinitrc >> ~/.xinitrc
fi

open -a X11 || open -a XDarwin

export "FONTCONFIG_PATH=$CWD/etc/fonts"
exec "$CWD/installer/velauncher.py"
