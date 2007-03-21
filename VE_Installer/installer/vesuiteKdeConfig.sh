#!/bin/sh

#This script sets up MIME-type and .desktop files for:
#1. Running VE-Suite when .ves files are double-clicked.
#2. Giving .ves files the VE-Suite icon.
#3. Inserting VE-Suite into the application menu.

#Run as an executable ( ./vesuiteKdeConfig.sh ), not in a "source" command.
#To work properly, a path to velauncher.py must be
#set in PATH in your ~/.cshrc file.
#After you're finished setting it up, re-login to put the changes into effect.

#NOTE: This script only works with KDE desktop.

#Files
export Mime_Type_Package_File=ves.desktop
export Desktop_File=vesuite.desktop
export Icon_File=gnome-mime-application-x-vesuite.png
#Location of Files
export Mime_Type_Package_File_Location=$Mime_Type_Package_File
export Desktop_File_Location=$Desktop_File
export Icon_File_Location=installerImages/$Icon_File
#Directories to Install Files To
export Mime_Directory=$HOME/.kde/share/mimelnk
export Applications_Directory=$HOME/.kde/share/applnk
export Icon_Directory=$HOME/.kde/share/icons

##Copy MimeType package & update Mime database
echo "Installing MIME-type package for VE-Suite..."
mkdir -p $Mime_Directory/application
cp $Mime_Type_Package_File_Location $Mime_Directory/application/$Mime_Type_Package_File
#update-mime-database $Mime_Directory

##Install Desktop package
echo "Installing .desktop file for VE-Suite..."
mkdir -p $Applications_Directory
#desktop-file-install --vendor=VRAC --dir=$Applications_Directory --rebuild-mime-info-cache $Desktop_File_Location

##Install icon file
echo "Installing icon for .ves files..."
mkdir -p $Icon_Directory
cp $Icon_File_Location $Icon_Directory/vesuite.png

echo "Done."
echo "Re-login for settings to take effect."
