#!/bin/sh

#This script sets up MIME-type and .desktop files for:
#1. Running VE-Suite when .ves files are double-clicked.
#2. Giving .ves files the VE-Suite icon.
#3. Inserting VE-Suite into the application menu.

#Run as an executable ( ./vesuiteGnomeConfig.sh ), not in a "source" command.
#Valid Arguments:
# --user: User-only configuration
# --global: Global configuration
# (Nothing): Default configuration
#           (Global if write-access to /usr/share/, else user-only.)

#To work properly, a path to velauncher.py must be
#set in PATH in your ~/.cshrc file.
#After you're finished setting it up, re-login to put the changes into effect.

#NOTE: This script only works with Gnome 2.8+ desktop.

#Files
export Mime_Type_Package_File=ves.desktop
export Desktop_File=vesuite.desktop
export Reboot_File=vesuiteReboot.desktop
export Shutdown_File=vesuiteShutdown.desktop
export Wake_File=vesuiteWake.desktop
export Mimetype_Icon_File=gnome-mime-application-x-vesuite.png
export Menu_Icon_Source=ve_icon32x32.png
export Menu_Icon_Destination=ve-suite.png
#Location of Files
export Mime_Type_Package_File_Location=$Mime_Type_Package_File
export Desktop_File_Location=$Desktop_File
export Reboot_File_Location=$Reboot_File
export Shutdown_File_Location=$Shutdown_File
export Wake_File_Location=$Wake_File
export Mimetype_Icon_File_Location=installerImages/$Mimetype_Icon_File
export Menu_Icon_File_Location=installerImages/$Menu_Icon_Source
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
      "")
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

#Set Install Directories
if [ $GlobalConfig == "True" ]
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
   rm -f $Mime_Directory/application/$Mime_Type_Package_File
else
   echo "Installing MIME-type package for VE-Suite..."
   mkdir -p $Mime_Directory/application
   cp $Mime_Type_Package_File_Location $Mime_Directory/application/$Mime_Type_Package_File
fi
#update-mime-database $Mime_Directory

##Install Desktop package
if [ $Uninstall == "True" ]
then
   echo "Uninstalling .desktop file for VE-Suite..."
   rm -f $Applications_Directory/vrac-$Desktop_File
   rm -f $Applications_Directory/vrac-$Reboot_File
   rm -f $Applications_Directory/vrac-$Shutdown_File
   rm -f $Applications_Directory/vrac-$Wake_File
   update-desktop-database $Applications_Directory
else
   echo "Installing .desktop file for VE-Suite..."
   mkdir -p $Applications_Directory
   desktop-file-install --vendor=vrac --dir=$Applications_Directory --rebuild-mime-info-cache $Desktop_File_Location
   desktop-file-install --vendor=vrac --dir=$Applications_Directory --rebuild-mime-info-cache $Reboot_File_Location
   desktop-file-install --vendor=vrac --dir=$Applications_Directory --rebuild-mime-info-cache $Shutdown_File_Location
   desktop-file-install --vendor=vrac --dir=$Applications_Directory --rebuild-mime-info-cache $Wake_File_Location
fi

##Install icon file
if [ $Uninstall == "True" ]
then
   echo "Uninstalling icon for .ves files..."
   rm -f $Icon_Directory/hicolor/48x48/mimetypes/$Mimetype_Icon_File
   rm -f $Icon_Directory/hicolor/32x32/apps/$Menu_Icon_Destination
else
   echo "Installing icon for .ves files..."
   mkdir -p $Icon_Directory/hicolor/48x48/mimetypes
   mkdir -p $Icon_Directory/hicolor/32x32/apps
   cp $Mimetype_Icon_File_Location $Icon_Directory/hicolor/48x48/mimetypes/$Mimetype_Icon_File
   cp $Menu_Icon_File_Location $Icon_Directory/hicolor/32x32/apps/$Menu_Icon_Destination
fi

echo "Done."
echo "Re-login for settings to take effect."
