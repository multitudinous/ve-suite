call ..\setup.bat
cd /D %VE_SUITE_HOME%\VE_Installer\installer
set PYTHONPATH=%VE_SUITE_HOME%\VE_Installer\installer\python
python setup.py py2exe -b 1
