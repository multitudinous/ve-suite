SET( LIB_NAME osgdb_sgb )

ADD_OSGPLUGIN( ${LIB_NAME}
    ReaderWriterSGB.cpp
    ReaderWriterSGB.h
)

INCLUDE( ModuleInstall REQUIRED )
