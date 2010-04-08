// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include <osg/Notify>
#include <osgDB/Registry>
#include <osgwTools/Version.h>

#include <string>
#include <ostream>



// Works around a shortcoming in the osgDB::Registry. If there are two different
// plugins supporting two different formats that both use the same extension (such
// as ".sgb") then one of them needs to be loaded explicitly in order for read
// operations to always succeed. This class explicitly loads a .osgb plugin. We
// don't really care which one it finds and loads, as long as it succeeds. A
// static variable is declared below. The constructor should be invoked when the
// executable and osgBulletPlus shared library are loaded, before main() starts to
// execite.
//
class PluginLoader
{
public:
    PluginLoader()
    {
        const std::string libName( osgDB::Registry::instance()->createLibraryNameForExtension( "sgb" ) );
        std::ostream& ostr( osg::notify( osg::INFO ) );

#if( OSGWORKS_OSG_VERSION >= 20800 )
        osgDB::Registry::LoadStatus stat( osgDB::Registry::instance()->loadLibrary( libName ) );
        ostr << ".osgb plugin lib name: \"" << libName << "\" ";
        switch( stat ) {
        case osgDB::Registry::NOT_LOADED:
            ostr << " NOT_LOADED" << std::endl;
            break;
        case osgDB::Registry::PREVIOUSLY_LOADED:
            ostr << " PREVIOUSLY_LOADED" << std::endl;
            break;
        case osgDB::Registry::LOADED:
            ostr << " LOADED" << std::endl;
            break;
        default:
            ostr << " Unknown load status" << std::endl;
            break;
        }
#else
        // No Registry::LoadStatus before OSG v2.8.
        bool stat( osgDB::Registry::instance()->loadLibrary( libName ) );
        ostr << ".skeleton plugin lib name: \"" << libName << "\" " <<
            (stat ? "Loaded" : "Failed to load") << std::endl;
#endif
    }
    ~PluginLoader()
    {
        //std::ostream& ostr( osg::notify( osg::INFO ) );
        //ostr << "~PluginLoader." << std::endl;
    }
};

static PluginLoader s_pluginLoader;
