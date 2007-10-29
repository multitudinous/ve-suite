#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a string in specific files contained in current directory

#echo "number of arguments is" $#argv 
#echo "first arg is" $1
#echo "second arg is" $2
#echo "third arg is" $3

if ( $#argv == 1 ) then
   foreach file ( $1 )
      echo ""
      echo $file
      mv $file $file.old
      sed -f cmdfile "$file.old" > "$file"
      #rm -f $file.old
   end
   echo ""
else if ( $#argv == 3 ) then
   foreach file ( $1 )
      echo ""
      echo $file
      mv $file $file.old
      sed "s/$2/$3/g" "$file.old" > "$file"
      #rm -f $file.old
   end
   echo ""
else if ( $#argv == 2 ) then
   foreach file ( $1 )
      echo ""
      echo $file
      mv $file $file.old
      sed "s/$2/RCSfile: $file/g" "$file.old" > "$file"
      rm -f $file.old
   end
   echo ""
else
   echo "   usage: findAndReplace 'filePattern' findString replaceString"
   echo "    i.e., findAndReplace '*.c* *.h' 1993 1994"
   echo "    or "
   echo "    this replaces the filename in the .cxx or .h file with the real filename "
   echo "    findAndReplace '*.c* *.h' 'RCSfile: filename'"
   echo ""
endif

