#######################################################
# this are setting used in SETUP_EXAMPLE macro
#######################################################
SET(TARGET_DEFAULT_PREFIX "appl_")
SET(TARGET_DEFAULT_LABEL_PREFIX "Application")

SET(TARGET_COMMON_LIBRARIES 
    OpenThreads
    osg 
    osgDB 
    osgUtil
    osgGA
    osgViewer 
    osgText
    osgBullet
    LibBulletDynamics
    LibBulletCollision
    LibBulletMath
    LibBulletColladaConverter 
)

IF(DYNAMIC_OPENSCENEGRAPH)
    ADD_SUBDIRECTORY(osgbpp)
    ADD_SUBDIRECTORY(handpreprocess)
ENDIF(DYNAMIC_OPENSCENEGRAPH)

