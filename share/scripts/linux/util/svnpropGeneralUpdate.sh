#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a set of text (specifed in cmdfile) in specific files
# contained in current directory

# get the list of files that contain the old header
set containingHeader = `find . \( -name "*.py" -or -name "*.cpp" -or -name "*.h" -or -name "*.cxx" \)`

foreach file ( $containingHeader )
   echo "$file - Activate props"
   dos2unix $file
   svn propset svn:keywords "Date Author Revision Id" $file
   svn propset svn:eol-style native $file
end

set containingHeader = `find . \( -name "*.jconf" -or -name "*.ves" -or -name "*.vea" -or -name "*.vem" \)`

foreach file ( $containingHeader )
   echo "$file - Activate props"
   dos2unix $file
   #svn propset svn:keywords "Date Author Revision Id" $file
   svn propset svn:eol-style native $file
   svn propset svn:mime-type text/xml $file
end
