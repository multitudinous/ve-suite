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

// --- VE-Suite Includes --- //
#include "MSAccessConnection.h"

// --- POCO Includes --- //

// --- C/C++ Includes --- //

////////////////////////////////////////////////////////////////////////////////
MSAccessConnection::MSAccessConnection(
    std::string& db,
    std::string& server,
    std::string& username,
    std::string& password,
    unsigned int port )
    :
    DBConnection( db )
{
    m_dbType = MSACCESS;
}
////////////////////////////////////////////////////////////////////////////////
MSAccessConnection::~MSAccessConnection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MSAccessConnection::QueryTables()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
/*
const Poco::Data::RecordSet* const MSAccessConnection::GetTableFieldNames(
    std::string& tableName )
{
    return NULL;
}
*/
////////////////////////////////////////////////////////////////////////////////
const StringVector2D* const MSAccessConnection::GetTableDetails(
    std::string& tableName )
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
/*
const Poco::Data::RecordSet* const MSAccessConnection::GetTableData(
    std::string& tableName )
{
    return NULL;
}
*/
////////////////////////////////////////////////////////////////////////////////
