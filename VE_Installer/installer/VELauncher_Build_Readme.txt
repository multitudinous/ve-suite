==========
Building a Standalone VELauncher for Windows
==========

1. Make sure the build computer has the latest versions of Python and wxPython installed.
2. Download and install Py2exe from:
http://www.py2exe.org/
3. On the command line, enter velauncher.py's directory.
4. Call the setup.py script with:
python setup.py py2exe -b 1
5. This will create velauncher.exe in the directory, a standalone Windows version of VELauncher.