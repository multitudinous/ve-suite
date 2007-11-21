/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date: 2007-11-11 15:55:24 -0600 (Sun, 11 Nov 2007) $
 * Version:       $Rev: 9860 $
 * Author:        $Author: dshipton $
 * Id:            $Id: StreamLineEventHandler.cxx 9860 2007-11-11 21:55:24Z dshipton $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/event/viz/StreamLineEventHandler.h>

#include <ves/xplorer/SteadyStateVizHandler.h>

#include <ves/xplorer/GlobalBase.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <boost/filesystem/operations.hpp>   //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace ves::xplorer::event;

////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler::StreamLineEventHandler()
:
ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler::StreamLineEventHandler( const StreamLineEventHandler& rhs )
:
ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler::~StreamLineEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void StreamLineEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void StreamLineEventHandler::Execute( ves::open::xml::XMLObject* veXMLObject )
{
    ves::open::xml::Command* command = dynamic_cast< ves::open::xml::Command* >( veXMLObject );
    ves::open::xml::DataValuePairWeakPtr sizeDVP = command->GetDataValuePair( "Size" );
    ves::open::xml::DataValuePairWeakPtr glowDVP = command->GetDataValuePair( "Glow" );

    double size, glow;
    if( sizeDVP )
    {
        sizeDVP->GetData( size );
    }
    
    if( glowDVP )
    {
        glowDVP->GetData( glow );
    }

    //ves::xplorer::SteadyStateVizHandler::instance()->

   /*
        if( _activeObject )
    {
        ves::xplorer::cfdStreamers* temp = dynamic_cast< ves::xplorer::cfdStreamers* >( _activeObject );
        if( temp )
        {
            ;//temp->Update
        }
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
StreamLineEventHandler& StreamLineEventHandler::operator=( const StreamLineEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
