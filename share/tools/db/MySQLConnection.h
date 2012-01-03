/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef MY_SQL_CONNECTION_H
#define MY_SQL_CONNECTION_H

// --- VE-Suite Includes --- //
#include "DBConnection.h"

// --- POCO Includes --- //
#include <Poco/SharedPtr.h>

namespace Poco
{
namespace Data
{
class Statement;
}
}

// --- C/C++ Includes --- //

/*!\file MySQLConnection.h
 *
 */

/*!\class MySQLConnection
 * 
 */
class MySQLConnection : public DBConnection
{
public:
    ///Constructor
    MySQLConnection(
        std::string& db,
        std::string& server,
        std::string& username,
        std::string& password,
        unsigned int port );

    ///Destructor
    virtual ~MySQLConnection();

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableFieldNames(
        std::string& tableName );
    */

    ///
    virtual const StringVector2D* const GetTableDetails(
        std::string& tableName );

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableData(
        std::string& tableName );
    */

protected:
    ///
    virtual void QueryTables();

private:
    Poco::SharedPtr< Poco::Data::Statement > m_statement;

};

#endif //MY_SQL_CONNECTION_H
