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
#include <eventmanager/ScopedConnectionList.h>


namespace eventmanager
{
struct null_deleter
{
    void operator()(void*) {}
};

ScopedConnectionList::ScopedConnectionList()
    :
    this_(this, null_deleter())
{
}

ScopedConnectionList::ScopedConnectionList( const ScopedConnectionList& )
    :
    this_(this, null_deleter())
{
}

ScopedConnectionList::~ScopedConnectionList( )
{
}

void ScopedConnectionList::AddConnection( boost::shared_ptr< boost::signals2::scoped_connection > connection )
{
    mConnections.push_back( connection );
}

void ScopedConnectionList::DropConnections( )
{
    // Since shared pointers automatically delete when they go out of scope,
    // we can simply clear the list to cause the connections to autodelete
    mConnections.clear( );
}

ScopedConnectionList::ConnectionList_type::iterator ScopedConnectionList::GetBegin( )
{
    return mConnections.begin();
}

ScopedConnectionList::ConnectionList_type::iterator ScopedConnectionList::GetEnd( )
{
    return mConnections.end();
}

boost::shared_ptr< boost::signals2::scoped_connection > ScopedConnectionList::GetLastConnection( )
{
    ConnectionList_type::iterator iter = mConnections.end( );
    if( iter != mConnections.begin( ) )
    {
        iter--; // Decrement because mConnections.end() returns one position *past* the last element
        return (*iter );
    }
    else
    {
        // If there are no existing connections, return an empty one.
        boost::shared_ptr< boost::signals2::scoped_connection > emptyConnection( new boost::signals2::scoped_connection );
        return emptyConnection;
    }
}

}

