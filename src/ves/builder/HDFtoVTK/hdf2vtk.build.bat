REM location of VTK
REM Modify the below variable to point to local libraries
set VTK_HOME=C:\usr\local
set HDF5_INC_PATH=C:\HDF\hdf5-1.6.3-patch\hdf5lib\debug\include
set HDF5_LIB_PATH=C:\HDF\hdf5-1.6.3-patch\hdf5lib\debug\lib
set HDF4_INC_PATH=C:\HDF\HDF4.2r0\include
set HDF4_LIB_PATH=C:\HDF\HDF4.2r0\libdbg
set SZIP_INC_PATH=C:\szip\src
set SZIP_LIB_PATH=C:\szip\all\lib\Debug
set ZLIB_INC_PATH=C:\zlib
set ZLIB_LIB_PATH=C:\zlib\projects\visualc6\Win32_DLL_ASM_Debug
set VE_SUITE_HOME=C:\VE_Suite_101404\
set Path=%Path%;%VTK_HOME%\bin
REM only change this if your Visual Studio .NET is installed somewhere else
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv.exe" HDFtoVTK.sln 
cmd
