call ..\setup.bat
cd /D %VE_SUITE_HOME%\VE_Installer\installer
set PYTHONPATH=%VE_SUITE_HOME%\VE_Installer\installer\python
del python\*.pyc
python setup.py py2exe --dll-exclude MFC71.dll -b 1
