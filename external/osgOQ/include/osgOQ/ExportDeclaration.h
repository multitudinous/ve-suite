//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#ifndef OSGOQ_EXPORT_DECLARATION_H
#define OSGOQ_EXPORT_DECLARATION_H 1

#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined( __BCPLUSPLUS__)  || defined( __MWERKS__)
    #  if defined( OSGOQ_LIBRARY_STATIC )
    #    define OSGOQ_EXPORT
    #  elif defined( OSGOQ_EXPORTS )
    #    define OSGOQ_EXPORT   __declspec(dllexport)
    #  else
    #    define OSGOQ_EXPORT   __declspec(dllimport)
    #  endif
#else
    #  define OSGOQ_EXPORT
#endif  

#endif
