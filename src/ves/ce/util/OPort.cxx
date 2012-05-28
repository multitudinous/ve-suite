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
#include <ves/open/xml/Command.h>
#include <ves/ce/util/OPort.h>

using namespace VE_CE::Utilities;
////////////////////////////////////////////////////////////////////////////////
OPort::OPort( int id, Module* m )
    : Port( id, m ), _profile( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
OPort::OPort( const OPort& p )
    : Port( p )
{
    copy( p );
}
////////////////////////////////////////////////////////////////////////////////
OPort::~OPort()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void OPort::copy( const OPort& p )
{
    if( this == &p )
    {
        return;
    }

    if( _profile )
    {
        delete _profile;
    }
    _profile = new Types::Profile( *( p._profile ) );

    *data = *( p.data );
}
////////////////////////////////////////////////////////////////////////////////
int OPort::have_data( void )
{
    return ( data != NULL );
}
////////////////////////////////////////////////////////////////////////////////
int OPort::have_profile( void )
{
    return ( _profile != NULL );
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::CommandPtr OPort::GetPortData( void )
{
    return data;
}
////////////////////////////////////////////////////////////////////////////////
void OPort::SetPortData( ves::open::xml::CommandPtr inputData )
{
    data = inputData;
}
