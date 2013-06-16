@echo OFF
set OSG_FILE_PATH=%CD%/share/vesuite
set OSG_FILE_PATH=%OSG_FILE_PATH%;%CD%/share/backdropFX/data
set OSG_FILE_PATH=%OSG_FILE_PATH%;%CD%/share/osgBullet/data
set OSG_FILE_PATH=%OSG_FILE_PATH%;%CD%/share/osgWorks/data
set OSG_FILE_PATH=%OSG_FILE_PATH%;%CD%/share/latticefx/data

set Path=%CD%/bin;%CD%/lib;%CD%/lib/osgPLugins-2.8.5;%CD%/lib64;%Path%
ves_xplorer --jconf=%CD%/share/vesuite/vrj_configs/stereo_desktop/desktop.jconf --VESDesktopMode --VESRTT
