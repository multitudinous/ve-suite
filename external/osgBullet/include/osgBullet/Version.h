// Copyright 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#ifndef __OSGBULLET_VERSION_H__
#define __OSGBULLET_VERSION_H__ 1

#include "osgBullet/Export.h"
#include <string>


namespace osgBullet {


#define OSGBULLET_MAJOR_VERSION 1
#define OSGBULLET_MINOR_VERSION 0
#define OSGBULLET_SUB_VERSION 0

// C preprocessor integrated version number.
// The form is Mmmss, where:
//   M is the major version
//   mm is the minor version (zero-padded)
//   ss is the sub version (zero padded)
// Use this in version-specific code, for example:
//   #if( OSGBULLET_VERSION < 10500 )
//      ... code specific to releases before v1.05
//   #endif
#define OSGBULLET_VERSION ( \
        ( OSGBULLET_MAJOR_VERSION * 10000 ) + \
        ( OSGBULLET_MINOR_VERSION * 100 ) + \
          OSGBULLET_SUB_VERSION )

// Returns OSGBULLET_VERSION.
unsigned int OSGBULLET_EXPORT getVersionNumber();

// Pretty string.
std::string OSGBULLET_EXPORT getVersionString();


// namespace osgBullet
}

// __OSGBULLET_VERSION_H__
#endif
