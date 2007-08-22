call ..\setupXP.bat
cd /D %VE_SUITE_HOME%\VE_Installer\installer
echo %VE_SUITE_HOME%
set PYTHONPATH=%VE_SUITE_HOME%\VE_Installer\installer\python
del python\*.pyc
python setup.py py2exe --dll-exclude MFC71.dll -b 1
