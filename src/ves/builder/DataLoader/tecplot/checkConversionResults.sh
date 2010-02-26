#!/bin/csh

# convert each file in the ascii_data directory...
foreach file ( ./ascii_data/* )
    #echo $file 
    ./tecplotReader --outputToCurrentDir $file
end

# drop down into the directory with the solution files...
cd vtu_results

# comnpare each output file to the "gold standard" in the vtu_results directory...
#echo ""
echo "Comparing output files"

foreach file ( *.vtu )
    echo $file 
    diff $file ../$file
end

# if the call to this script uses no arguments then delete the newly converted files...
if ( $# == 0 ) then
    echo ""
    echo "Deleting newly converted vtu files"
    foreach file ( *.vtu )
        rm ../$file
    end
endif

# return to the original directory...
cd ..

echo "Done"


