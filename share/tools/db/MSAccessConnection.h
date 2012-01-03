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

#ifndef MS_ACCESS_CONNECTION_H
#define MS_ACCESS_CONNECTION_H

// --- VE-Suite Includes --- //
#include "DBConnection.h"

// --- POCO Includes --- //

// --- wxWidgets Includes --- //
/*
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32
*/

// --- C/C++ Includes --- //

/*!\file MSAccessConnection.h
 *
 */

/*!\class MSAccessConnection
 * 
 */
class MSAccessConnection : public DBConnection
{
public:
    ///Constructor
    MSAccessConnection(
        std::string& db,
        std::string& server,
        std::string& username,
        std::string& password,
        unsigned int port );

    ///Destructor
    virtual ~MSAccessConnection();

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

};

#endif //MS_ACCESS_CONNECTION_H
