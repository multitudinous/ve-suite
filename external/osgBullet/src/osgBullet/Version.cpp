// Copyright 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#include "osgBullet/Version.h"
#include <string>
#include <sstream>

namespace osgBullet {


unsigned int
getVersionNumber()
{
    return( OSGBULLET_VERSION );
}


static std::string s_osgbullet_version( "" );

std::string
getVersionString()
{
    if( s_osgbullet_version.empty() )
    {
        std::ostringstream oStr;
        oStr << std::string( "osgBullet version " ) <<
            OSGBULLET_MAJOR_VERSION << "." <<
            OSGBULLET_MINOR_VERSION << "." <<
            OSGBULLET_SUB_VERSION << " (" <<
            getVersionNumber() << ").";
        s_osgbullet_version = oStr.str();
    }
    return( s_osgbullet_version );
}


// namespace osgBullet
}
