#!/bin/sh

##echo $PWD ##TESTER

export Mime_Type_Package=VE-Suite.xml
export Desktop_File=vesuite.desktop
export Mime_Directory=$HOME/.local/share/mime
export Applications_Directory=$HOME/.local/share/applications

##Copy MimeType package & update Mime database
cp $Mime_Type_Package $Mime_Directory/packages/$Mime_Type_Package
update-mime-database $Mime_Directory

##Install Desktop package
desktop-file-install --vendor=VRAC --dir=$Applications_Directory --rebuild-mime-info-cache $Desktop_File
