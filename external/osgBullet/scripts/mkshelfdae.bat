@echo off

rem Use one of these two lines to control whether osgbpp does its post-generatrion display of the physics data.
set DISPLAY=--display
rem set DISPLAY=

@echo on
osgbpp %DISPLAY% --triMesh --mass 0 --overall -o USMC23_4000.ASM1.dae USMC23_4000.ASM.ive

osgbpp %DISPLAY% --triMesh --overall -o USMC30_4000.ASM1.dae USMC30_4000.ASM.ive
