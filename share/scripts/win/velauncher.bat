@echo OFF
set OSG_FILE_PATH=%CD%/share/vesuite;%CD%/share/backdropFX/data
set OSG_FILE_PATH=%OSG_FILE_PATH%;%CD%/share/osgBullet/data;%CD%/share/osgWorks/data

set Path=%CD%/bin;%CD%/lib;%CD%/lib64;%CD%;%Path%
ves_xplorer --jconf=%CD%/share/vesuite/vrj_configs/stereo_desktop/desktop.jconf --VESDesktopMode --VESRTT