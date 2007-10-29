#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a set of text (specifed in cmdfile) in specific files
# contained in current directory

# get the list of files that contain the old header
set containingHeader = `find . \( -name "*.py" -or -name "*.cxx" -or -name "*.h" \) -exec grep -L "VE-Suite is (C) Copyright 199" {} \;`

foreach file ( $containingHeader )
   echo "$file - Add header and activate props"
   mv $file $file.old
   sed -f $VE_SUITE_HOME/VE_Installer/scripts/cmdheader "$file.old" > "$file"
   rm -f $file.old
   svn propset svn:keywords "Date Author Revision Id" $file
end
#   if [ `grep -lr "VE-Suite is (C) Copyright 199" $file` == "" ]
#   then
#   
