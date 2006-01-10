#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# finds and replaces a set of text (specifed in cmdfile) in specific files
# contained in current directory

# get the list of files that contain the header
set containingHeader = `grep -l "auto-copyright.pl BEGIN" *`

foreach file ( $containingHeader )
   echo ""
   echo $file
   mv $file $file.old
   sed -f /home/users/sjk60/scripts/cmdfile "$file.old" > "$file"
   rm -f $file.old
end

