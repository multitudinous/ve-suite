//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#ifndef OSGWXTREE_EXPORT_
#define OSGWXTREE_EXPORT_    1

#if defined ( WIN32 ) && !( defined ( __CYGWIN__ ) || defined ( __MINGW32__ ) )
 #pragma warning( disable : 4244 )
 #pragma warning( disable : 4251 )
 #pragma warning( disable : 4267 )
 #pragma warning( disable : 4275 )
 #pragma warning( disable : 4290 )
 #pragma warning( disable : 4786 )
 #pragma warning( disable : 4305 )
 #pragma warning( disable : 4996 )
#endif

#if defined ( _MSC_VER ) || defined ( __CYGWIN__ ) || defined ( __MINGW32__ ) || defined ( __BCPLUSPLUS__ ) || defined ( __MWERKS__ )
 #if defined ( OSG_LIBRARY_STATIC )
  #define OSGWXTREE_EXPORT
 #elif defined ( OSGWXTREE_LIBRARY )
  #define OSGWXTREE_EXPORT    __declspec( dllexport )
 #else
  #define OSGWXTREE_EXPORT    __declspec( dllimport )
 #endif
#else
 #define OSGWXTREE_EXPORT
#endif

#endif
