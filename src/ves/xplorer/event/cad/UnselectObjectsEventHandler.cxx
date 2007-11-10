/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/event/cad/UnselectObjectsEventHandler.h>

#include <ves/xplorer/cfdGlobalBase.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

using namespace ves::xplorer::event;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
UnselectObjectsEventHandler::UnselectObjectsEventHandler()
:
ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnselectObjectsEventHandler::UnselectObjectsEventHandler( const UnselectObjectsEventHandler& rhs )
:
ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnselectObjectsEventHandler::~UnselectObjectsEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UnselectObjectsEventHandler::SetGlobalBaseObject( ves::xplorer::cfdGlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UnselectObjectsEventHandler::Execute( XMLObject* veXMLObject )
{
    Command* command = dynamic_cast< Command* >( veXMLObject );

    if( command->GetCommandName() == "UNSELECT_OBJECTS" )
    {
        ves::xplorer::DeviceHandler::instance()->UnselectObjects();
    }
}
////////////////////////////////////////////////////////////////////////////////
UnselectObjectsEventHandler& UnselectObjectsEventHandler::operator=( const UnselectObjectsEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
