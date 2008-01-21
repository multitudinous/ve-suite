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
#include <ves/xplorer/volume/SQLiteDriver.h>
#include <ves/xplorer/volume/Database.h>

#include <sqlite3.h>

#include <string>
#include <vector>

namespace VE_TextureBased
{
namespace
{
/**
 * Class that ensures the registration of the SQLiteDriver to
 * Database.
 */
/// the driver instance used by the create function.
static SQLiteDriver                     CreateDriver;
Database_t::DriverCreator Creator( &CreateDriver, &VE_TextureBased::SQLiteDriver::create );
struct RegisterSQLiteDriver
{
    RegisterSQLiteDriver()
    {
        Database::Instance().registerDriver( "SQLite3", Creator );
    }
};
RegisterSQLiteDriver    RegisteringDriver;

}

bool
SQLiteDriver::open( const std::string& name )
{
    if( name == mName )
    {
        return true;
    }
    if( mOpen )
    {
        close();
    }
    int rc = sqlite3_open( name.c_str(), &mConnection );
    if( rc )
    {
        mOpen = false;
        return false;
    }
    mName = name;
    mOpen = true;
    return true;
}

void
SQLiteDriver::close()
{
    if( mConnection && mOpen )
    {
        sqlite3_close( mConnection );
        mConnection = NULL;
        mOpen = false;
    }
}

bool
SQLiteDriver::execute( const std::string& query )
{
    if( !mConnection || !mOpen )
    {
        return false;
    }
    // Compile the query into a statement.
    sqlite3_stmt* stmt = NULL;
    const char* tail = NULL;
    if( sqlite3_prepare( mConnection, query.c_str(), -1, &stmt, &tail ) !=
            SQLITE_OK )
    {
        return false;
    }
    // Statement prepared; now step through its results.
    bool finished = false;
    mResults.clear();
    while( !finished )
    {
        int return_code = sqlite3_step( stmt );
        if( SQLITE_DONE == return_code )
        {
            finished = true;
        }
        else if( SQLITE_ROW == return_code )
        {
            // We got a row back; add the results in.
            std::vector<DBValue> row;
            int column_cnt = sqlite3_column_count( stmt );
            for( int i = 0; i < column_cnt; ++i )
            {
                int column_type = sqlite3_column_type( stmt, i );
                switch ( column_type )
                {
                    case SQLITE_INTEGER:
                        row.push_back( DBValue( sqlite3_column_int64( stmt, i ) ) );
                        break;
                    case SQLITE_FLOAT:
                        row.push_back( DBValue( sqlite3_column_double( stmt, i ) ) );
                        break;
                    case SQLITE_TEXT:
                    {
                        std::string s =
                            reinterpret_cast<const char*>(
                                sqlite3_column_text( stmt, i ) );
                        row.push_back( DBValue( s ) );
                        break;
                    }
                    case SQLITE_BLOB:
                    {
                        BinaryData bd( sqlite3_column_bytes( stmt, i ),
                                       const_cast<unsigned char*>(
                                           reinterpret_cast<const unsigned char*>
                                           ( sqlite3_column_blob( stmt, i ) ) ) );
                        row.push_back( DBValue( bd ) );
                        break;
                    }
                    case SQLITE_NULL:
                        row.push_back( DBValue( DBNullValue() ) );
                        break;
                    default:
                        return false;
                }
            }
            mResults.push_back( row );
        }
    }
    return true;
}
}
