@echo off

rem Use one of these two lines to control whether osgbpp does its post-generatrion display of the physics data.
set DISPLAY=--display
rem set DISPLAY=

@echo on
osgbpp %DISPLAY% --box --overall -o USMC23_4019.ASM.0.dae USMC23_4019.ASM.ive
osgbpp %DISPLAY% --box -o USMC23_4019.ASM.1.dae USMC23_4019.ASM.ive

osgbpp %DISPLAY% --triMesh --overall --mass 0 -o USMC23_4006.ASM.1.dae USMC23_4006.ASM.ive

osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4142.PRT.1.dae USMC23_4142.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4143.PRT.1.dae USMC23_4143.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4115.PRT.1.dae USMC23_4115.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4158-1.PRT.1.dae USMC23_4158-1.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4158.PRT.1.dae USMC23_4158.PRT.ive
osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_4159.PRT.1.dae USMC23_4159.PRT.ive
osgbpp %DISPLAY% --triMesh --overall --mass 0 -o USMC23_3175.PRT.1.dae USMC23_3175.PRT.ive
