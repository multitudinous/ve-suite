@echo on

rem uses the osgWorks geometryop test

rem doorframe
geometryop %1 usmc23_1019.asm.ive

rem simple version of Doug's crash
DecimatorDemo %1 usmc23_4126.prt.ive
rem full version of Doug's crash -- the entire container
geometryop %1 USMC23_4024.ASM.ive

rem drawer
geometryop %1 USMC23_4009.ASM.ive
