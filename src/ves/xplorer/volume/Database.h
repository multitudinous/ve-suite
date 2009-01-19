/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#ifndef JPG_DATABASE_H_
#define JPG_DATABASE_H_

#include <ves/xplorer/volume/Data.h>
#include <ves/xplorer/volume/DatabaseDriver.h>
#include <ves/xplorer/volume/SingletonDLL.h>
#include <ves/VEConfig.h>

#define LOKI_SINGLETON_EXPORT VE_TEXTURE_BASED_EXPORTS
#include <loki/Function.h>
#include <loki/Singleton.h>

#include <map>
#include <string>
#include <vector>

namespace VE_TextureBased
{
/**
 * Implementation of the Database Singleton.  This class handles a single
 * connection to a database; it delegates most functionality out to a
 * DatabaseDriver.
 */
class VE_TEXTURE_BASED_EXPORTS Database_t
{
public:

    /**
     * Default Ctor
     * Initializes the driver to NULL.
     */
    Database_t()
            : mCurrentDriver( NULL )
    {}

    /**
     * Dtor
     * Closes and deletes the current driver if necessary.
     */
    ~Database_t()
    {
        if( mCurrentDriver )
        {
            mCurrentDriver->close();
        }
    }

    /**
     * Opens a connection to the given database.
     *
     * @param   name     the name of the database to open a connection to.
     *
     * @return     true if succesful, false if not.
     */
    bool open( const std::string& name )
    {
        if( !mCurrentDriver )
        {
            return false;
        }
        return mCurrentDriver->open( name );
    }

    /**
     * Closes the connection to the database.
     */
    void close()
    {
        if( mCurrentDriver && mCurrentDriver->isOpen() )
        {
            mCurrentDriver->close();
        }
    }

    /**
     * Executes a SQL statement
     *
     * @param   statement      the SQL statement to execute.
     *
     * @return     true if successful, false otherwise
     */
    bool execute( const std::string& statement )
    {
        if( mCurrentDriver && mCurrentDriver->isOpen() )
        {
            return mCurrentDriver->execute( statement );
        }
        return false;
    }

    /**
     * Retrieves the results from the last query.
     *
     * @return     the results from the last query
     */
    std::vector< std::vector<DBValue> > getResults() const
    {
        if( mCurrentDriver )
        {
            return mCurrentDriver->getResults();
        }
    }

    /**
     * Queries the open status of the database.
     *
     * @return     true if the database is open, false otherwise.
     */
    bool isOpen()
    {
        if( mCurrentDriver )
        {
            return mCurrentDriver->isOpen();
        }
        return false;
    }

    /**
     * Sets the current driver.
     *
     * @param   name     the name of the driver to use.
     *
     * @return     true if successful, false otherwise.
     */
    bool setDriver( const std::string& name )
    {
        if( mDriverMap.find( name ) != mDriverMap.end() )
        {
            if( mCurrentDriver )
            {
                mCurrentDriver->close();
                delete mCurrentDriver;
            }
            mCurrentDriver = mDriverMap[name]();
            return mCurrentDriver == NULL;
        }
        return false;
    }

    /**
     * Returns the name of the current driver.
     *
     * @return     the name of the current driver.
     */
    std::string getDriverName() const
    {
        if( mCurrentDriver )
        {
            return mCurrentDriver->getDriverName();
        }
        return "None";
    }

    /**
     * Returns the version of the current driver.
     *
     * @return     the version of the current driver.
     */
    std::string getDriverVersion() const
    {
        if( mCurrentDriver )
        {
            return mCurrentDriver->getDriverVersion();
        }
        return "N/A";
    }

    /// driver creation functors.
    typedef Loki::Function < DatabaseDriver*() > DriverCreator;

    /**
     * Registers a driver with the database.
     *
     * @param   name     the name of the driver.
     * @param   creator  the function used to create a new instance of the
     *                   driver.
     */
    void registerDriver( const std::string& name,
                         const DriverCreator& creator )
    {
        mDriverMap[name] = creator;
    }

private:

    /// the current driver in use.
    DatabaseDriver*                                 mCurrentDriver;

    /// the map of driver names to their creation functions.
    std::map<std::string, DriverCreator>            mDriverMap;
};

/// Typedef for the singleton declaration.  This is necessary to make the
/// singleton have one instance in a Windows DLL; the macro calls are
/// based upon recommendations from the MSDN documentation.
#ifdef WIN32
VE_TEXTURE_BASED_TEMPLATE_EXPORTS template class VE_TEXTURE_BASED_EXPORTS Singleton<Database_t>;
#endif
/// Typedef for the singleton Database.
typedef Singleton<Database_t> Database;
}

#endif
