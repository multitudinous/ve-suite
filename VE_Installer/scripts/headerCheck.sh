#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a set of text (specifed in cmdfile) in specific files
# contained in current directory

# get the list of files that contain the old header
set noHeaders = `find . \( -name "*.py" -or -name "*.cxx" -or -name "*.h" \) -exec grep -L "VE-Suite is (C) Copyright 199" {} \;`

echo "NO HEADERS:"
foreach file ( $noHeaders )
   echo "$file"
end
