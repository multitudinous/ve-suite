@echo off
rem
rem This batch file runs osgbpp to create multiple DAE
rem files for the tetra.osg and cow.osg models. You can
rem load these files with the colladaread example.
rem For example:
rem   colladaread cow.osg
rem then press 0, 1, or 2 to load cow0.dae, cow1.dae,
rem or cow2.dae respectively.
rem Or:
rem   colladaread tetra.osg
rem and hit the 0 or 1 key to load either of the two files.
rem
rem After the files are loaded and the physics data extracted,
rem the data is added to the Bullet simulation and the cow or
rem tetrahedron should fall to the ground.
rem

rem Use one of these two lines to control whether osgbpp does its post-generatrion display of the physics data.
set DISPLAY=--display
rem set DISPLAY=

@echo on
osgbpp %DISPLAY% --box --overall -o tetra0.dae tetra.osg
osgbpp %DISPLAY% --triMesh --overall -o tetra1.dae tetra.osg

osgbpp %DISPLAY% --box --overall -o offcube0.dae offcube.osg
osgbpp %DISPLAY% --triMesh --overall -o offcube1.dae offcube.osg

osgbpp %DISPLAY% --box --overall -o cow0.dae cow.osg
osgbpp %DISPLAY% --triMesh --simplify .2 --overall -o cow1.dae cow.osg
osgbpp %DISPLAY% --triMesh --overall -o cow2.dae cow.osg

osgbpp %DISPLAY% --box --overall --mass 0 -o USMC23_1019.ASM0.dae USMC23_1019.ASM.ive
osgbpp %DISPLAY% --triMesh --overall --mass 0 -o USMC23_1019.ASM1.dae USMC23_1019.ASM.ive

osgbpp %DISPLAY% --box --overall -o USMC23_1020.ASM0.dae USMC23_1020.ASM.ive
osgbpp %DISPLAY% --triMesh --overall -o USMC23_1020.ASM1.dae USMC23_1020.ASM.ive
