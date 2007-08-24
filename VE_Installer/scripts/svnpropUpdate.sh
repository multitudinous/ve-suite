#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a set of text (specifed in cmdfile) in specific files
# contained in current directory

# get the list of files that contain the old header
set containingHeader = `find . \( -name "*.py" -or -name "*.cxx" -or -name "*.h" \) -exec grep -l "VE-Suite is (C) Copyright 1998-2007" {} \;`

foreach file ( $containingHeader )
   echo "$file - Activate props"
   dos2unix $file
   svn propset svn:keywords "Date Author Revision Id" $file
   svn propset svn:eol-style native $file
end
