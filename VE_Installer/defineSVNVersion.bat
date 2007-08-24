set SVN_VES_REVISION=999999
REM Get the current directory
set VES_BUILD_DIR=%CD%

REM Go to VES home
cd %VE_SUITE_HOME%

REM Check svn version
svnversion > version.txt

REM Pipe the output into the env variable
set /p SVN_VES_REVISION= < version.txt
del version.txt

echo %SVN_VES_REVISION%

REM Restore current directory
chdir /d %VES_BUILD_DIR% 