#!/bin/bash
#
# Running this script will attempt to convert all CFD and FEA files in current
# directory first to tecplot plt format and then to VTK vtu format

# Where is this script (and the tecplot-to-vtk executable) located? 
TECPLOTREADER_DIR=`dirname $0`

# for each file ...
for filename in *
do
    #echo $filename 

    # read the extension of the filename ...
    extension=${filename##*.}

    # export filename (without extension) for use within the macro ...
    export SJKFILENAME=${filename%.*}

    # Depending on file extension, convert to tecplot plt format ...
    # (the -b flag instructs Tecplot 360 to run in batch mode)
    if [ $extension = rst ]
    then
        /Applications/Tec360_2009/bin/tec360 -b ${TECPLOTREADER_DIR}/mcr/ansys.mcr
    elif [ $extension = inp ]
    then
        /Applications/Tec360_2009/bin/tec360 -b ${TECPLOTREADER_DIR}/mcr/abaqus.mcr
    elif [ $extension = cas ]
    then
        /Applications/Tec360_2009/bin/tec360 -b ${TECPLOTREADER_DIR}/mcr/fluent.mcr
    else
        #echo "   Can not handle files with extension" $extension
        continue
    fi

    # convert tecplot format to vtk ...
    ${TECPLOTREADER_DIR}/tecplotReader ${filename%.*}.plt
    
done

unset SJKFILENAME

