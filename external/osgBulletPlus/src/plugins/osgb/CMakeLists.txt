SET( LIB_NAME osgdb_osgb )

ADD_OSGPLUGIN( ${LIB_NAME}
    ReaderWriterOSGB.cpp
    ReaderWriterOSGB.h
)

INCLUDE( ModuleInstall REQUIRED )
