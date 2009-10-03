SET( LIB_NAME osgdb_osgBullet )

ADD_OSGPLUGIN( ${LIB_NAME}
    dotosgCreationRecord.cpp
    dotosgPhysicsData.cpp
)

INCLUDE( ModuleInstall REQUIRED )
