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
#include <ves/xplorer/network/UpdateNetworkEventHandler.h>
#include <ves/xplorer/network/cfdExecutive.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;
using namespace ves::xplorer::network;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
UpdateNetworkEventHandler::UpdateNetworkEventHandler()
        : ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UpdateNetworkEventHandler::UpdateNetworkEventHandler( const UpdateNetworkEventHandler& rhs )
        : ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
UpdateNetworkEventHandler::~UpdateNetworkEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
UpdateNetworkEventHandler& UpdateNetworkEventHandler::operator=( const UpdateNetworkEventHandler& rhs )
{
    if( this != &rhs )
    {
        UpdateNetworkEventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void UpdateNetworkEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    ;
}
//////////////////////////////////////////////////////////////////////////
void UpdateNetworkEventHandler::Execute( XMLObject* xmlObject )
{
    Command* cmd = static_cast< Command* >( xmlObject );
    if( cmd->GetDataValuePair( "Load Data" ) )
    {
        cfdExecutive::instance()->LoadDataFromCE();
    }
}
