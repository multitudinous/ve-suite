#!/bin/sh

#This script builds the linux executable for velauncher.py.
#Requires pyinstaller ( http://pyinstaller.python-hosting.com/ )
#OUTPUT: A velauncher executable in ${Output_Dir}.

#Location of PyInstaller
export PyInstaller_Location=/home/vr/Applications/TSVEG/Build_Apps/pyinstaller-1.3
#Directory the executable will be written to.
export Output_Dir=dist

#Configures PyInstaller
python ${PyInstaller_Location}/Configure.py
#Makes velauncher's spec file.
python ${PyInstaller_Location}/Makespec.py --onefile --out=${Output_Dir} velauncher.py
#Builds the executable
python ${PyInstaller_Location}/Build.py ${Output_Dir}/velauncher.spec
#Afterwards, grab the executable from ${Output_Dir} and place it in VE_Suite's bin directory.
