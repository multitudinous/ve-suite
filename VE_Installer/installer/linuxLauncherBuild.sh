#!/bin/sh

#This script builds the linux executable for velauncher.py.
#Run it, then grab the velauncher executable from ${Output_Dir}.

export PyInstaller_Location=/home/vr/Applications/TSVEG/Build_Apps/pyinstaller-1.3
export Output_Dir=dist

python ${PyInstaller_Location}/Configure.py
python ${PyInstaller_Location}/Makespec.py --onefile --out=${Output_Dir} velauncher.py
python ${PyInstaller_Location}/Build.py ${Output_Dir}/velauncher.spec
