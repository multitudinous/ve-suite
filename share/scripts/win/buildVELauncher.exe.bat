call ..\setupXP.bat
cd /D %VE_SUITE_HOME%\VE_Installer\installer
echo %VE_SUITE_HOME%
set PYTHONPATH=%VE_SUITE_HOME%\VE_Installer\installer\python
set INSTIMGDIR=%VE_SUITE_HOME%\VE_Installer\installer\installerImages
set FREEZEOUTDIR=

set PYDIR=%VE_SUITE_HOME%\external\FreezePython
set PYINSTDIR=%PYDIR%\pyinstaller-1.3
set UPX=%PYDIR%\upx300w
set PYOUTDIR=%VE_SUITE_HOME%

set PATH=%PATH%;%UPX%;%PYINSTDIR%

python %PYINSTDIR%\Configure.py
python %PYINSTDIR%\Makespec.py --onefile --path=%PYTHONPATH% --icon=%INSTIMGDIR%\Ve_icon.ico --noconsole --upx velauncher.py
python %PYINSTDIR%\Build.py velauncher.spec