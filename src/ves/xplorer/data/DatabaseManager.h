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

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //
#include <boost/noncopyable.hpp>
#include <boost/signals2/signal.hpp>

// --- C++ headers --- //
#include <string>
#include <vector>

#include <ves/VEConfig.h>

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
class VE_DATA_EXPORTS DatabaseManager
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

    /**
     * Returns a vector of strings of the data contained in
     * column @c columnName in table @c tableName where search criteria 
     * @c searchCriteria is satisfied. If the data in the column is
     * not string data, this method attempts a lexical cast to string. If the 
     * cast fails, an empty vector is returned.
     * @param tableName The name of the table in the database
     * @param columnName The name of the column containing the desired data
     * @param searchCriteria Predicate for an SQL WHERE clause. For example,
     * if @c searchCriteria is "Foo = 'Bar'" then only results where the contents of
     * column Foo are equal to the string Bar will be returned. If you want a list of all 
     * data in @c columnName, set @c searchCriteria to a null string ("").
     * @c searchCriteria must conform to the grammar of SQL WHERE predicates; in
     * short, WHERE predicates are of the form @code column_name operator value 
     * @endcode where @c column_name is the name of the column to look in, 
     * @c operator is one of: [=,<>,>,<,>=,<=,BETWEEN,LIKE,IN] (<> means not equal),
     * and @c value is the value to be compared using @c operator. String values
     * MUST be enclosed in single quotes; numeric values MUST NOT.
     * @param distinct If true, no duplicate values will appear in the returned 
     * vector. Defualt value is false.
     * @return Vector of strings
     */
    std::vector< std::string > GetStringVector( const std::string& tableName, 
                                                const std::string& columnName, 
                                                const std::string& searchCriteria = "", 
                                                bool distinct = false );

    /**
     * Checks whether table with @c tableName exists in the current database.
     * @param tableName Name of table to check for
     * @return @c true if table exists, @c false otherwise.
     */
    bool TableExists( const std::string& tableName );

    void ResetAll();
    
    void Shutdown();

    bool SaveAs( const std::string& path );

    bool LoadFrom( const std::string& path );

private:
    /// ctor
    DatabaseManager( );

    /// dtor
    virtual ~DatabaseManager( );

    /// Singleton declarations
    vprSingletonHeader( DatabaseManager );

    /// Holds the session pool
    Poco::Data::SessionPool* mPool;

    /// Holds the current db path
    std::string m_path;

    /// Signal registered as "DatabaseManager.ResyncFromDatabase" that is
    /// emitted during calls to LoadFrom.
    boost::signals2::signal< void() > m_resyncFromDatabase;
};

}// namespace data
}// namespace xplorer
}// namespace ves
