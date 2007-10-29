#!/bin/sh

#This script sets up MIME-type and .desktop files for:
#1. Running VE-Suite when .ves files are double-clicked.
#2. Giving .ves files the VE-Suite icon.
#3. Inserting VE-Suite into the application menu.

#Run as an executable ( ./vesuiteGnomeConfig.sh ), not in a "source" command.
#Valid Arguments:
# --gnome: Sets GNOME as the main desktop.
# --kde: Sets KDE as the main desktop.
# NOTE: If neither argument is passed, GNOME is the main desktop by default.
# --both: Installs both GNOME & KDE configurations. You still need to use
#         --gnome or --kde to set the main desktop of the two.
# --user: User-only configuration.
# --global: Global configuration.
# --uninstall: Uninstalls the files this configuration installed.
# (Nothing): Default configuration.
#           (Global if write-access to /usr/share/, else user-only.
#            GNOME set to main desktop.)

#To work properly, a path to velauncher.py must be
#set in PATH in your ~/.cshrc file.
#After you're finished setting it up, re-login to put the changes into effect.

#NOTE: This script works with Gnome 2.8+ desktops and KDE desktops.

export Base_Directory=`pwd`
export Desktop_Base_Directory=${Base_Directory}/LinuxConfig/desktops
export Icon_Base_Directory=${Base_Directory}/LinuxConfig/icons
export Mimetype_Base_Directory=${Base_Directory}/LinuxConfig/mimetypes
export Desktop_Path=~/Desktop
export Vendor=vrac
#Set default input arguments.
export Primary_Desktop=None
export Uninstall=False
export Gnome=False
export Kde=False
#Get input arguments.
for InputArgument in $1 $2 $3 $4
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
         #Uninstall
         export Uninstall=True
         ;;
      --gnome)
         #GNOME's the primary desktop
         export Primary_Desktop=Gnome
         export Gnome=True
         ;;
      --kde)
         #KDE's the primary desktop
         export Primary_Desktop=Kde
         export Kde=True
         ;;
      --both)
         #Install both GNOME & KDE.
         export Gnome=True
         export Kde=True
         ;;
      *)
         #Invalid arg. Spit out error message and quit.
         echo ""
         echo "$InputArgument is not a valid argument."
         echo "Specify your main desktop with --gnome or --kde."
         echo "If you want it set up to run on both, pass '--both' as well." 
         echo "Try '--user' or '--global' to choose the configuration's scope."
         echo "Try '--uninstall' to uninstall the configuration."
         echo "EX: Pass '--gnome --both --global' to install for both desktops,"
         echo "    using GNOME as your primary, for all users."
         echo "EX: Pass '--gnome --both --global --uninstall' to uninstall the"
         echo "    above installation."
         echo ""
         exit 0
         ;;
   esac
done
if [ $Primary_Desktop == "None" ]
then
      export Primary_Desktop=Gnome
      export Gnome=True
fi
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
   export Gnome_Mime_Directory=/usr/share/mime
   export Kde_Mime_Directory=/usr/share/mimelnk
   export Gnome_Icon_Directory=/usr/share/icons
   export Kde_Icon_Directory=/usr/share/icons
   if [ "$Primary_Desktop" == "Gnome" ]
   then
      echo "Primary desktop: Gnome"
      ##export Mime_Directory=/usr/share/mime
      export Applications_Directory=/usr/share/applications
      ##export Icon_Directory=/usr/share/icons
   else
      echo "Primary desktop: Kde"
      ##export Mime_Directory=/usr/share/mimelnk
      export Applications_Directory=/usr/share/applnk
      ##export Icon_Directory=/usr/share/icons
   fi
else
   echo "Setting up user-only configuration."
   export Gnome_Mime_Directory=$HOME/.local/share/mime
   export Kde_Mime_Directory=$HOME/.kde/share/mimelnk
   export Gnome_Icon_Directory=$HOME/.icons
   export Kde_Icon_Directory=$HOME/.kde/share/icons
   if [ "$Primary_Desktop" == "Gnome" ]
   then
      echo "Primary desktop: Gnome"
      ##export Mime_Directory=$HOME/.local/share/mime
      export Applications_Directory=$HOME/.local/share/applications
      ##export Icon_Directory=$HOME/.icons
   else
      echo "Primary desktop: Kde"
      ##export Mime_Directory=$HOME/.kde/share/mimelnk
      export Applications_Directory=$HOME/.kde/share/applnk
      ##export Icon_Directory=$HOME/.kde/share/icons
   fi
fi
if [ "$Gnome" == "True" ]
then
   if [ "$Kde" == "True" ]
   then
      echo "Installing for both Gnome & Kde."
   fi
fi

##Copy MimeType package & update Mime database
if [ $Uninstall == "True" ]
then
   if [ "$Gnome" == "True" ]
   then
      cd $Mimetype_Base_Directory
      export Mimetype_File_List=`find . -name "*.xml"`
      echo "Uninstalling Gnome MIME-type package for VE-Suite..."
      cd $Gnome_Mime_Directory/packages
      rm -fv $Mimetype_File_List
      update-mime-database $Gnome_Mime_Directory
      cd $Base_Directory
   fi
   if [ "$Kde" == "True" ]
   then
      cd $Mimetype_Base_Directory
      export Mimetype_File_List=`find . -name "*.desktop"`
      echo "Uninstalling Kde MIME-type package for VE-Suite..."
      cd $Kde_Mime_Directory/application
      rm -fv $Mimetype_File_List      
      cd $Base_Directory
   fi
else
   if [ "$Gnome" == "True" ]
   then
      echo "Installing Gnome MIME-type package for VE-Suite..."
      mkdir -p $Gnome_Mime_Directory/packages
      cd $Mimetype_Base_Directory
      find . -name "*.xml" -print -exec cp --parents {} $Gnome_Mime_Directory/packages/ \;
      update-mime-database $Gnome_Mime_Directory
      cd $Base_Directory
   fi
   if [ "$Kde" == "True" ]
   then
      echo "Installing Kde MIME-type package for VE-Suite..."
      mkdir -p $Kde_Mime_Directory/application
      cd $Mimetype_Base_Directory
      find . -name "*.desktop" -print -exec cp --parents {} $Kde_Mime_Directory/application/ \;
      cd $Base_Directory
   fi
fi

##Install Desktop package
if [ $Uninstall == "True" ]
then
   echo "Uninstalling ${Primary_Desktop} .desktop files for VE-Suite..."
   cd $Desktop_Base_Directory
   export Desktop_File_List=`find . -name "*.desktop" -printf ${Vendor}-%f\\\\n`
   cd $Applications_Directory
   rm -fv $Desktop_File_List
   update-desktop-database $Applications_Directory
else
   echo "Installing ${Primary_Desktop} .desktop files for VE-Suite..."
   cd $Desktop_Base_Directory
   find . -name "*.desktop" -print -exec desktop-file-install --vendor=$Vendor --dir=$Applications_Directory --rebuild-mime-info-cache {} \;
fi
cd $Base_Directory

##Install icon files
if [ $Uninstall == "True" ]
then
   cd $Icon_Base_Directory
   export Icon_File_List=`find . -name "*.png"`
   if [ $Gnome == "True" ]
   then
      echo "Uninstalling Gnome icons for .ves files..."
      cd $Gnome_Icon_Directory
      rm -fv $Icon_File_List
   fi
   if [ $Kde == "True" ]
   then
      echo "Uninstalling Kde icons for .ves files..."
      cd $Kde_Icon_Directory
      rm -fv $Icon_File_List
   fi
   cd $Base_Directory
else
   cd $Icon_Base_Directory
   if [ $Gnome == "True" ]
   then
      echo "Installing Gnome icons for .ves files..."
      find . -name "*.png" -print -exec cp --parents {} $Gnome_Icon_Directory \;
   fi
   if [ $Kde == "True" ]
   then
      echo "Installing Kde icons for .ves files..."
      find . -name "*.png" -print -exec cp --parents {} $Kde_Icon_Directory \;
   fi
   cd $Base_Directory
fi

##Make desktop shortcuts
if [ $Uninstall == "True" ]
then
   echo "Removing desktop files on desktop..."
   cd $Desktop_Base_Directory
   export Desktop_File_List=`find . -name "*.desktop"`
   cd $Desktop_Path
   rm -fv $Desktop_File_List
elif [ "$GlobalConfig" == "False" ]
then
   echo "Making desktop links on desktop..."
   cd $Desktop_Base_Directory
   find . -name "*.desktop" -print -exec cp {} $Desktop_Path \;
fi
cd $Base_Directory

echo "Done."
echo "Re-login for settings to take effect."
