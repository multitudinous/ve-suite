// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLETPLUS_EXPORT_H__
#define __OSGBULLETPLUS_EXPORT_H__    1

#if defined ( WIN32 ) && !( defined ( __CYGWIN__ ) || defined ( __MINGW32__ ) )
 //#pragma warning( disable : 4244 )
 //#pragma warning( disable : 4251 )
 //#pragma warning( disable : 4267 )
 //#pragma warning( disable : 4275 )
 //#pragma warning( disable : 4290 )
 //#pragma warning( disable : 4786 )
 //#pragma warning( disable : 4305 )
 //#pragma warning( disable : 4996 )
#endif

#if defined ( _MSC_VER ) || defined ( __CYGWIN__ ) || defined ( __MINGW32__ ) || defined ( __BCPLUSPLUS__ ) || defined ( __MWERKS__ )
 #if defined ( OSG_LIBRARY_STATIC )
  #define OSGBULLETPLUS_EXPORT
 #elif defined ( OSGBULLETPLUS_LIBRARY )
  #define OSGBULLETPLUS_EXPORT    __declspec( dllexport )
 #else
  #define OSGBULLETPLUS_EXPORT    __declspec( dllimport )
 #endif
#else
 #define OSGBULLETPLUS_EXPORT
#endif

// __OSGBULLETPLUS_EXPORT_H__
#endif
