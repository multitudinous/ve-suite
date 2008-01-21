/*************** <auto-copyright.rb BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef JPG_DATABASE_DRIVER_H_
#define JPG_DATABASE_DRIVER_H_

#include <ves/xplorer/volume/Data.h>

#include <string>
#include <vector>

namespace VE_TextureBased
{
/**
 * The interface that all database drivers must adhere to.  A database
 * driver can be thought of as both the bridge between an application and
 * the Database API and the cursor that the application uses to execute
 * queries and retrieve results.
 */
class DatabaseDriver
{
public:

    virtual ~DatabaseDriver()
    {}

    /**
     * Opens a connection to the database via this driver.
     *
     * @param   name     the name of the database to open.
     *
     * @return     true if sucessful, false otherwise.
     */
    virtual bool open( const std::string& name ) = 0;

    /**
     * Closes a connection to the database.
     */
    virtual void close() = 0;

    /**
     * Executes the given query.
     *
     * @param   statement   the SQL query statement to execute.
     *
     * @return     true if successful, false otherwise
     */
    virtual bool execute( const std::string& query ) = 0;

    /**
     * Gets the results from the most recent query.
     *
     * @return     the results from the most recent query.
     */
    virtual std::vector< std::vector<DBValue> > getResults() const = 0;

    /**
     * Queries the open status of this database
     *
     * @return     true if the database is open, false otherwise.
     */
    virtual bool isOpen() const = 0;

    /**
     * Gets the name of this database driver.
     */
    virtual std::string getDriverName() const = 0;

    /**
     * Gets the version of this database driver.
     */
    virtual std::string getDriverVersion() const = 0;
};
}
#endif
