call D:\devEnv\VES\share\scripts\win\setupXP.bat
cd /D %VE_SUITE_HOME%\src\apps\launcher
echo %VE_SUITE_HOME%
set PYTHONPATH=%VE_SUITE_HOME%\src\apps\launcher\python
set INSTIMGDIR=%VE_SUITE_HOME%\dist\installerImages
set FREEZEOUTDIR=%VE_SUITE_HOME%\install\release\bin

set PYDIR=%VE_SUITE_HOME%\external\FreezePython
set PYINSTDIR=%PYDIR%\pyinstaller-1.3
set UPX=%PYDIR%\upx300w
set PYOUTDIR=%VE_SUITE_HOME%

set PATH=%PATH%;%UPX%;%PYINSTDIR%

python %PYINSTDIR%\Configure.py
python %PYINSTDIR%\Makespec.py --onefile --path=%PYTHONPATH% --icon=%INSTIMGDIR%\Ve_icon.ico --noconsole --upx velauncher.py
python %PYINSTDIR%\Build.py velauncher.spec
