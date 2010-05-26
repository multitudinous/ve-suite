/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //
#include <boost/noncopyable.hpp>

// --- C++ headers --- //
#include <string>

// Forward declarations
namespace Poco
{
namespace Data
{
class Session;
class SessionPool;
}
}

namespace ves
{
namespace xplorer
{
namespace data
{

///
///@class DatabaseManager
/// Simple singleton that maintains a pool of SQLite sessions connected to
/// the application's main database. This allows an easy, centralized way to
/// manage connections to the database and to change the path of the database
/// file in a single place.
class DatabaseManager
{
public:
    ///
    /// Sets the path (including filename) of the database file.
    /// @param path Path of the database file
    void SetDatabasePath( const std::string& path );

    ///
    /// Returns a pointer to the session pool. Callers can get a valid session
    /// like so:
    /// @code
    /// Poco::Data::Session mySession( GetPool()->get() );
    /// @endcode
    /// The session created in this way will be automatically returned to the
    /// SessionPool when mySession goes out of scope.
    Poco::Data::SessionPool* GetPool();
    
private:
    /// ctor
    DatabaseManager( );

    /// dtor
    virtual ~DatabaseManager( );

    /// Singleton declarations
    vprSingletonHeader( DatabaseManager );

    /// Holds the session pool
    Poco::Data::SessionPool* mPool;
};

}// namespace data
}// namespace xplorer
}// namespace ves