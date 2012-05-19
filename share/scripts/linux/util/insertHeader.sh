#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a set of text (specifed in cmdfile) in specific files
# contained in current directory

# get the list of files that contain the old header
set containingHeader = `find . \( -name "*.py" -or -name "*.cpp" -or -name "*.h" \) -exec grep -L "latticeFX is (C) Copyright 20" {} \;`

foreach file ( $containingHeader )
   echo "$file - Add header and activate props"
   mv $file $file.old
   sed -f $VE_SUITE_HOME/share/scripts/linux/util/cmdheader "$file.old" > "$file"
   rm -f $file.old
   svn propset svn:keywords "Date Author Revision Id" $file
end
#   if [ `grep -lr "VE-Suite is (C) Copyright 199" $file` == "" ]
#   then
#   
