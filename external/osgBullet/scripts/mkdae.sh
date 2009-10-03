#!/bin/bash

# This batch file runs osgbpp to create multiple DAE
# files for the tetra.osg and cow.osg models. You can
# load these files with the colladaread example.
# For example:
#   colladaread cow.osg
# then press 0, 1, or 2 to load cow0.dae, cow1.dae,
# or cow2.dae respectively.
# Or:
#   colladaread tetra.osg
# and hit the 0 or 1 key to load either of the two files.
#
# After the files are loaded and the physics data extracted,
# the data is added to the Bullet simulation and the cow or
# tetrahedron should fall to the ground.

# Use one of these two lines to control whether osgbpp does its post-generatrion display of the physics data.
# export DISPLAY=--display
export DISPLAY=

osgbpp ${DISPLAY} --box --overall -o tetra0.dae tetra.osg
osgbpp ${DISPLAY} --convexHull --overall -o tetra1.dae tetra.osg

osgbpp ${DISPLAY} --box --overall -o offcube0.dae offcube.osg
osgbpp ${DISPLAY} --convexHull --overall -o offcube1.dae offcube.osg

osgbpp ${DISPLAY} --box --overall -o concave0.dae concave.osg
osgbpp ${DISPLAY} --box -o concave1.dae concave.osg

osgbpp ${DISPLAY} --box --overall -o cow0.dae cow.osg
osgbpp ${DISPLAY} --convexHull --simplify .2 --overall -o cow1.dae cow.osg
osgbpp ${DISPLAY} --convexHull --overall -o cow2.dae cow.osg


rem doorframe
osgbpp ${DISPLAY} --box --overall --mass 0 -o USMC23_1019.ASM0.dae USMC23_1019.ASM.ive
osgbpp ${DISPLAY} --triMesh --overall --mass 0 -o USMC23_1019.ASM1.dae USMC23_1019.ASM.ive
osgbpp ${DISPLAY} --triMesh --mass 0 -o USMC23_1019.ASM2.dae USMC23_1019.ASM.ive

rem door
osgbpp ${DISPLAY} --box --overall -o USMC23_1020.ASM0.dae USMC23_1020.ASM.ive
osgbpp ${DISPLAY} --convexHull --overall -o USMC23_1020.ASM1.dae USMC23_1020.ASM.ive
osgbpp ${DISPLAY} --convexHull -o USMC23_1020.ASM2.dae USMC23_1020.ASM.ive


rem drawer
osgbpp ${DISPLAY} --box --overall -o USMC23_4019.ASM0.dae USMC23_4019.ASM.ive
osgbpp ${DISPLAY} --box -o USMC23_4019.ASM1.dae USMC23_4019.ASM.ive

rem drawer
osgbpp ${DISPLAY} --box -o USMC23_4009.ASM1.dae USMC23_4009.ASM.ive

rem door support, part of the cabinet
osgbpp ${DISPLAY} --triMesh --overall --mass 0 -o USMC23_4006.ASM1.dae USMC23_4006.ASM.ive

rem other cabinet parts
osgbpp ${DISPLAY} --box --overall --mass 0 -o USMC23_4142.PRT1.dae USMC23_4142.PRT.ive
osgbpp ${DISPLAY} --box --overall --mass 0 -o USMC23_4143.PRT1.dae USMC23_4143.PRT.ive
osgbpp ${DISPLAY} --box --overall --mass 0 -o USMC23_4115.PRT1.dae USMC23_4115.PRT.ive
osgbpp ${DISPLAY} --box --overall --mass 0 -o USMC23_4158-1.PRT1.dae USMC23_4158-1.PRT.ive
osgbpp ${DISPLAY} --box --overall --mass 0 -o USMC23_4158.PRT1.dae USMC23_4158.PRT.ive
osgbpp ${DISPLAY} --box --overall --mass 0 -o USMC23_4159.PRT1.dae USMC23_4159.PRT.ive
osgbpp ${DISPLAY} --triMesh --overall --mass 0 -o USMC23_3175.PRT1.dae USMC23_3175.PRT.ive


rem shelf
osgbpp ${DISPLAY} --triMesh --mass 0 --overall -o USMC23_4000.ASM1.dae USMC23_4000.ASM.ive

rem jack
osgbpp ${DISPLAY} --box --overall -o USMC30_4000.ASM0.dae USMC30_4000.ASM.ive
osgbpp ${DISPLAY} --convexHull -o USMC30_4000.ASM1.dae USMC30_4000.ASM.ive
