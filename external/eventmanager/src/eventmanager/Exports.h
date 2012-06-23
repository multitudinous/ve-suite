/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#pragma once

#if defined(_MSC_VER)
#pragma warning( disable : 4503 )
#pragma warning( disable : 4251 )
#endif

#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined( __BCPLUSPLUS__)  || defined( __MWERKS__)
    #  if defined( EVENTMANAGER_LIBRARY_STATIC )
    #    define EVENTMANAGER_EXPORT
    #    define EVENTMANAGER_LOCAL
    #  elif defined( EVENTMANAGER_LIBRARY )
    #    define EVENTMANAGER_EXPORT   __declspec(dllexport)
    #    define EVENTMANAGER_LOCAL
    #  else
    #    define EVENTMANAGER_EXPORT   __declspec(dllimport)
    #    define EVENTMANAGER_LOCAL
    #  endif
#else
  #if __GNUC__ >= 4
    # if defined( EVENTMANAGER_LIBRARY_STATIC )
    #    define EVENTMANAGER_EXPORT
    #    define EVENTMANAGER_LOCAL
    # else
    #    define EVENTMANAGER_EXPORT   __attribute__ ((visibility ("default")))
    #    define EVENTMANAGER_LOCAL   __attribute__ ((visibility ("hidden")))
    # endif
  #else
    #  define EVENTMANAGER_EXPORT
    #  define EVENTMANAGER_LOCAL
  #endif
#endif
