#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# converts keywords from cvs style to svn style
foreach file (*.c* *.h)
	echo ""
	echo $file
   mv $file $file.old
   sed 's/Version:       $Revision:/Version:       $Rev:/g' "$file.old" > "$file"
   rm -f $file.old
   svn propset svn:keywords "Date Rev" $file
end

