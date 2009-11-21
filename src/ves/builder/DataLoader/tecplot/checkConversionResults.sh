#!/bin/csh

# convert each file in the ascii_data directory...
foreach file ( ./ascii_data/* )
    #echo $file 
    ./tecplotReader $file
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

# if the call to this script uses a single argument then delete the newly converted files...
if ( $# == 1 ) then
    echo ""
    echo "Deleting newly converted vtu files"
    foreach file ( *.vtu )
        rm ../$file
    end
endif

# return to the original directory...
cd ..

echo "Done"


