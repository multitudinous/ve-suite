ADD_DEFINITIONS(-DOSGDB_OSGOQ_EXPORTS)

ADD_LIBRARY( osgdb_osgOQ MODULE 
    IO_oqNode.cpp
)

LINK_INTERNAL( osgdb_osgOQ
    osgOQ
    osgDB
    osgUtil
    osg
    OpenThreads
)
