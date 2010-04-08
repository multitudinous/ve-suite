@echo off
rem This batch file runs osgbpp to create multiple DAE
rem files for the container CAD files. Various osgBulletPlus
rem tests and examples use these DAE files, such as mtphys.

rem Use one of these two lines to control whether osgbpp does its post-generatrion display of the physics data.
rem set DISPLAY=--display
set DISPLAY=


@echo on
rem doorframe
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_1019.ASM0.dae USMC23_1019.ASM.ive
osgbpp %DISPLAY% --triMesh --overall --mass 0 -o USMC23_1019.ASM1.dae USMC23_1019.ASM.ive
osgbpp %DISPLAY% --triMesh --mass 0 -o USMC23_1019.ASM2.dae USMC23_1019.ASM.ive

rem door
osgbpp %DISPLAY% --box --overall -o USMC23_1020.ASM0.dae USMC23_1020.ASM.ive
osgbpp %DISPLAY% --convexHull --overall -o USMC23_1020.ASM1.dae USMC23_1020.ASM.ive
osgbpp %DISPLAY% --convexHull -o USMC23_1020.ASM2.dae USMC23_1020.ASM.ive


rem drawer
osgbpp %DISPLAY% --box --overall -o USMC23_4019.ASM0.dae USMC23_4019.ASM.ive
osgbpp %DISPLAY% --box -o USMC23_4019.ASM1.dae USMC23_4019.ASM.ive

rem drawer
osgbpp %DISPLAY% --box -o USMC23_4009.ASM1.dae USMC23_4009.ASM.ive

rem door support, part of the cabinet
osgbpp %DISPLAY% --triMesh --overall --mass 0 -o USMC23_4006.ASM1.dae USMC23_4006.ASM.ive

rem other cabinet parts
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4142.PRT1.dae USMC23_4142.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4143.PRT1.dae USMC23_4143.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4115.PRT1.dae USMC23_4115.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4158-1.PRT1.dae USMC23_4158-1.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4158.PRT1.dae USMC23_4158.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4159.PRT1.dae USMC23_4159.PRT.ive
osgbpp %DISPLAY% --triMesh --overall --mass 0 -o USMC23_3175.PRT1.dae USMC23_3175.PRT.ive


rem shelf
osgbpp %DISPLAY% --triMesh --mass 0 --overall -o USMC23_4000.ASM1.dae USMC23_4000.ASM.ive

rem jack
osgbpp %DISPLAY% --box --overall -o USMC30_4000.ASM0.dae USMC30_4000.ASM.ive
osgbpp %DISPLAY% --convexHull -o USMC30_4000.ASM1.dae USMC30_4000.ASM.ive
