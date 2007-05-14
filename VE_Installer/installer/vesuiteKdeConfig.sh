#!/bin/sh

#This script sets up MIME-type and .desktop files for:
#1. Running VE-Suite when .ves files are double-clicked.
#2. Giving .ves files the VE-Suite icon.
#3. Inserting VE-Suite into the application menu.

#Run as an executable ( ./vesuiteGnomeConfig.sh ), not in a "source" command.
#Valid Arguments:
# --user: User-only configuration
# --global: Global configuration
# --uninstall: Uninstalls the files this configuration installed.
# (Nothing): Default configuration
#           (Global if write-access to /usr/share/, else user-only.)

#To work properly, a path to velauncher.py must be
#set in PATH in your ~/.cshrc file.
#After you're finished setting it up, re-login to put the changes into effect.

#NOTE: This script only works with KDE desktops.

export Base_Directory=`pwd`
export Desktop_Base_Directory=${Base_Directory}/LinuxConfig/desktops
export Icon_Base_Directory=${Base_Directory}/LinuxConfig/icons
export Mimetype_Base_Directory=${Base_Directory}/LinuxConfig/mimetypes
export Desktop_Path=~/Desktop
export Vendor=vrac
#Set GlobalConfig
#GlobalConfig determines whether a user-only or global config is made.
export Uninstall=False
for InputArgument in $1 $2
do
   case $InputArgument in
      --user)
         #User-only config
         export GlobalConfig=False
         ;;
      --global)
         #Global config
         export GlobalConfig=True
         ;;
      --uninstall)
         export Uninstall=True
         ;;
      *)
         #Invalid arg. Spit out error message and quit.
         echo "Invalid argument passed."
         echo "Try '--user' or '--global' to choose the configuration's scope."
         echo "Try '--uninstall' to uninstall the configuration."
         exit 0
         ;;
   esac
done
#Default config:
#Global if user can write to /usr/share/
#otherwise user-only.
if [ -z "$GlobalConfig" ]
then
   if [ -w "/usr/share/" ]
   then
      export GlobalConfig=True
   else
      export GlobalConfig=False
   fi
fi

#Set Install Directories
if [ "$GlobalConfig" == "True" ]
then
   echo "Setting up global configuration."
   export Mime_Directory=/usr/share/mimelnk
   export Applications_Directory=/usr/share/applnk
   export Icon_Directory=/usr/share/icons
else
   echo "Setting up user-only configuration."
   export Mime_Directory=$HOME/.kde/share/mimelnk
   export Applications_Directory=$HOME/.kde/share/applnk
   export Icon_Directory=$HOME/.kde/share/icons
fi

##Copy MimeType package & update Mime database
if [ $Uninstall == "True" ]
then
   echo "Uninstalling MIME-type package for VE-Suite..."
   cd $Mimetype_Base_Directory
   export Mimetype_File_List=`find . -name "*.desktop"`
   cd $Mime_Directory/application
   rm -fv $Mimetype_File_List
else
   echo "Installing MIME-type package for VE-Suite..."
   mkdir -p $Mime_Directory/application
   cd $Mimetype_Base_Directory
   find . -name "*.desktop" -print -exec cp --parents {} $Mime_Directory/application/ \;
fi
cd $Base_Directory

##Install Desktop package
if [ $Uninstall == "True" ]
then
   echo "Uninstalling .desktop file for VE-Suite..."
   cd $Desktop_Base_Directory
   export Desktop_File_List=`find . -name "*.desktop" -printf ${Vendor}-%f\\\\n`
   cd $Applications_Directory
   rm -fv $Desktop_File_List
   update-desktop-database $Applications_Directory
else
   echo "Installing .desktop file for VE-Suite..."
   cd $Desktop_Base_Directory
   find . -name "*.desktop" -print -exec desktop-file-install --vendor=$Vendor --dir=$Applications_Directory --rebuild-mime-info-cache {} \;
fi
cd $Base_Directory

##Install icon files
if [ $Uninstall == "True" ]
then
   echo "Uninstalling icon for .ves files..."
   cd $Icon_Base_Directory
   export Icon_File_List=`find . -name "*.png"`
   cd $Icon_Directory
   rm -fv $Icon_File_List
else
   echo "Installing icon for .ves files..."
   cd $Icon_Base_Directory
   find . -name "*.png" -print -exec cp --parents {} $Icon_Directory \;
fi
cd $Base_Directory

##Make desktop shortcuts
if [ $Uninstall == "True" ]
then
   echo "Removing desktop files on desktop."
   cd $Desktop_Base_Directory
   export Desktop_File_List=`find . -name "*.desktop"`
   cd $Desktop_Path
   rm -fv $Desktop_File_List
elif [ "$GlobalConfig" == "False" ]
then
   echo "Making desktop links on desktop."
   cd $Desktop_Base_Directory
   find . -name "*.desktop" -print -exec cp {} $Desktop_Path \;
fi
cd $Base_Directory

echo "Done."
echo "Re-login for settings to take effect."
