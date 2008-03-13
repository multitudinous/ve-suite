/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 * Date modified: $Date: 2008-02-18 14:37:44 -0600 (Mon, 18 Feb 2008) $
 * Version:       $Rev: 10639 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#include <ves/xplorer/event/environment/SetResetStartPositionEventHandler.h>
#include <ves/xplorer/CommandHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler::SetResetStartPositionEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler
::SetResetStartPositionEventHandler( const SetResetStartPositionEventHandler& ceh )
{}
////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler::~SetResetStartPositionEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler&
SetResetStartPositionEventHandler::operator=( const SetResetStartPositionEventHandler& rhs )
{
    if( &rhs != this )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void SetResetStartPositionEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SetResetStartPositionEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    try
    {
        CommandPtr command = boost::dynamic_pointer_cast<Command>( veXMLObject );

        if( command->GetDataValuePair( "LOAD_PFB_FILE" ) )
        {
            //command->GetDataValuePair( "LOAD_PFB_FILE" )->GetData( whichChild );
        }
        ves::xplorer::CommandHandler::instance()->SetXMLCommand( command );        
    }
    catch ( ... )
    {
        std::cout << "Error!!" << std::endl;
        std::cout << "SetResetStartPositionEventHandler::_operateOnNode()" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
