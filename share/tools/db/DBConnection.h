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

#ifndef DB_CONNECTION_H
#define DB_CONNECTION_H

// --- VE-Suite Includes --- //
#include "TypeDefs.h"

// --- POCO Includes --- //
#include <Poco/SharedPtr.h>

namespace Poco
{
namespace Data
{
class Session;
//class RecordSet;
}
}

// --- wxWidgets Includes --- //
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32

// --- C/C++ Includes --- //
#include <map>

/*!\file DBConnection.h
 *
 */

/*!\class DBConnection
 * 
 */
class DBConnection
{
public:
    ///Constructor
    DBConnection( std::string& name );

    ///Destructor
    virtual ~DBConnection();

    ///
    enum
    {
        MSACCESS = 1,
        MYSQL = 2
    };

    ///
    const bool Connected() const;

    ///
    const unsigned int GetDBType() const;

    ///
    const std::string& GetName() const;

    ///
    const StringVector1D& GetTableNames() const;

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableFieldNames(
        std::string& tableName ) = 0;
    */

    ///
    virtual const StringVector2D* const GetTableDetails(
        std::string& tableName ) = 0;

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableData(
        std::string& tableName ) = 0;
    */

protected:
    ///
    virtual void QueryTables() = 0;

    ///
    bool m_connected;

    ///
    unsigned int m_dbType;

    ///
    std::string m_name;

    ///
    StringVector1D m_tableNames;

    ///
    //std::map< std::string, Poco::Data::RecordSet > m_tableFieldNames;

    ///
    std::map< std::string, StringVector2D > m_tableDetails;

    ///
    //std::map< std::string, Poco::Data::RecordSet > m_tableData;

    ///
    Poco::SharedPtr< Poco::Data::Session > m_session;

private:



};

#endif //DB_CONNECTION_H
