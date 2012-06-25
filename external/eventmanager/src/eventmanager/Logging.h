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

// New logging tools based on Poco::Logger and Poco::LogStream

#include <Poco/Logger.h>
#include <Poco/LogStream.h>

#include <eventmanager/PointerTypes.h>

// Smart pointer-ized version of LogStream to automate memory management

namespace eventmanager
{
/// Typedef for the SmartPtr types.
typedef ClassPtrDef<Poco::LogStream>::type  LogStreamPtr;
}

#define _LOG_CALL( prio, msg ) do {if( m_logger.prio() ){ (*m_logStream).prio() << msg << std::endl; } } while( 0 )
#define _STATIC_LOG_CALL( prio, name, msg ) do { Poco::Logger& logger = Poco::Logger::get( name ); if( logger.prio() ){Poco::LogStream logstream( logger ); logstream.information() << msg << std::endl; } } while( 0 )

/// Use these logging macros in classes that have a dedicated m_logger and m_logStream.
/// This method has faster execution that the static method below since the log
/// is only requested once and a reference to it is stored in the class's m_logger
/// member.
#define LOG_FATAL( msg ) _LOG_CALL( fatal, msg )
#define LOG_CRITICAL( msg ) _LOG_CALL( critical, msg )
#define LOG_ERROR( msg ) _LOG_CALL( error, msg )
#define LOG_WARNING( msg ) _LOG_CALL( warning, msg )
#define LOG_NOTICE( msg ) _LOG_CALL( notice, msg )
#define LOG_INFO( msg ) _LOG_CALL( information, msg )

/// Use these logging macros for one-off log messages and inside static methods
/// or other places not associated with a class. The name argument is the name
/// of the log to use. The name is in the form parent.child.subchild.subsubchild
/// where everything after parent is optional. However, if requesting parent.child.subchild,
/// a log named parent.child must have been previously requested, otherwise the
/// log call fails silently. Likewise, if requesting parent.child, the parent log
/// must have been previously requested.
#define STATIC_LOG_FATAL( name, msg ) _STATIC_LOG_CALL( fatal, name, msg )
#define STATIC_LOG_CRITICAL( name, msg ) _STATIC_LOG_CALL( critical, name, msg )
#define STATIC_LOG_ERROR( name, msg ) _STATIC_LOG_CALL( error, name, msg )
#define STATIC_LOG_WARNING( name, msg ) _STATIC_LOG_CALL( warning, name, msg )
#define STATIC_LOG_NOTICE( name, msg ) _STATIC_LOG_CALL( notice, name, msg )
#define STATIC_LOG_INFO( name, msg ) _STATIC_LOG_CALL( information, name, msg )


// DEBUG and TRACE log messages will only be compiled in when EVENTMANAGER_DEBUG
// is defined. This allows the liberal use of DEBUG and TRACE messages to
// track down difficult bugs without affecting performace of production builds.
#if defined(EVENTMANAGER_DEBUG)
    #define LOG_DEBUG( msg ) _LOG_CALL( debug, msg )
    #define LOG_TRACE( msg ) _LOG_CALL( trace, msg )
    #define STATIC_LOG_DEBUG( name, msg ) _STATIC_LOG_CALL( debug, name, msg )
    #define STATIC_LOG_TRACE( name, msg ) _STATIC_LOG_CALL( trace, name, msg )
#else
    #define LOG_DEBUG( msg )
    #define LOG_TRACE( msg )
    #define STATIC_LOG_DEBUG( name, msg )
    #define STATIC_LOG_TRACE( name, msg )
#endif

