#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a set of text (specifed in cmdfile) in specific files
# contained in current directory

# get the list of files that contain the old header
set containingHeader = `grep -lr "VE-Suite is (C) Copyright 1998-2004 by Iowa State University" * | grep -v "\.svn"`

foreach file ( $containingHeader )
   echo ""
   echo $file
   mv $file $file.old
   sed "s/ \* VE-Suite is (C) Copyright 1998-2004 by Iowa State University/ \* VE-Suite is (C) Copyright 1998-2006 by Iowa State University/g" "$file.old" > "$file"
   rm -f $file.old
end

