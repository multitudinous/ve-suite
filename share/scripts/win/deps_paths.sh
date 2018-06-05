#!/bin/sh

paths=$(find $(cygpath --unix $USERPROFILE/dev/deps) -wholename "*/install-64-bit/bin/*.dll" -o -wholename "*/install-64-bit/lib/*.dll" | xargs dirname | sort | uniq | xargs cygpath --windows)

printf "%s\n" $paths | while read -r line
do
    echo "set PATH=%PATH%;$line"
done
