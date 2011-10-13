#/bin/bash

if [ $# -lt 1 ]
then
    echo "Please specify at least one file name"
    exit -1
fi

echo ""

for f in $*
do
    echo "Displaying XML header for file $f..."
    LINE_NUM=(`grep -n "<AppendedData" "$f" | tr ':' ' '`)
    #xx=`grep -n "</Unstr" $1`
    #echo ${xx%%:*}
    #echo "LINE_NUM =" $LINE_NUM
    #echo head -n $LINE_NUM $1
    #$LINE_NUM=$LINE_NUM-1
    x=`head -n $LINE_NUM "$f"`
    echo "$x"
    echo ""
done

exit 0
