/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

// New logging tools based on Poco::Logger and Poco::LogStream
#include <Poco/Logger.h>
#include <Poco/LogStream.h>
#include <ves/util/PointerTypes.h>

// Smart pointer-ized version of LogStream to automate memory management
namespace ves
{
namespace xplorer
{
/// Typedef for the SmartPtr types.
typedef ves::util::ClassPtrDef<Poco::LogStream>::type  LogStreamPtr;
}
}

//#define DECLARE_LOGGER Poco::Logger& m_Logger; LogStreamPtr m_LogStream
//#define CREATE_LOG_STREAM m_LogStream = LogStreamPtr( new Poco::LogStream::LogStream( m_Logger ) )

#define _LOG_CALL( prio, msg ) do {if( m_Logger.prio() ){ (*m_LogStream).prio() << msg << std::endl; } } while( 0 )

#define LOG_FATAL( msg ) _LOG_CALL( fatal, msg )
#define LOG_CRITICAL( msg ) _LOG_CALL( critical, msg )
#define LOG_ERROR( msg ) _LOG_CALL( error, msg )
#define LOG_WARNING( msg ) _LOG_CALL( warning, msg )
#define LOG_NOTICE( msg ) _LOG_CALL( notice, msg )
#define LOG_INFO( msg ) _LOG_CALL( information, msg )


// DEBUG and TRACE log messages will only be compiled in when _LOGGING_DEBUG
// is defined. This allows the liberal use of DEBUG and TRACE messages to
// track down difficult bugs without affecting performace of production builds.
// _LOGGING_DEBUG can either define via SCons for the entire build or can be
// individually defined in a single class before the include statement for
// this file.
#if defined(_DEBUG)
    #define LOG_DEBUG( msg ) _LOG_CALL( debug, msg )
    #define LOG_TRACE( msg ) _LOG_CALL( trace, msg )
#else
    #define LOG_DEBUG( msg )
    #define LOG_TRACE( msg )
#endif

